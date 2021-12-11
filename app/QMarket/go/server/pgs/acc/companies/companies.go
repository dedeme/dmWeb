// Copyright 19-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Companies accounting charts.
package companies

import (
	"fmt"
	"github.com/dedeme/QMarket/data/acc"
	"github.com/dedeme/QMarket/data/cts"
	"github.com/dedeme/QMarket/data/model"
	"github.com/dedeme/QMarket/data/nick"
	"github.com/dedeme/QMarket/data/server"
	"github.com/dedeme/QMarket/db/acc/diariesDb"
	"github.com/dedeme/QMarket/db/confTb"
	"github.com/dedeme/QMarket/db/dailyDb/dailyTb"
	"github.com/dedeme/QMarket/db/investorsTb"
	"github.com/dedeme/QMarket/db/nicksTb"
	"github.com/dedeme/QMarket/db/quotesDb"
	"github.com/dedeme/QMarket/db/serversTb"
	"github.com/dedeme/QMarket/lock"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
	"strings"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "list":
		rp := map[string]json.T{}
		lock.Run(func() {
			serversTb := serversTb.Read()
			svs, selected := serversTb.HistoricList()
			sv := svs[selected]
			confSv, _ := sv.HistoricConf()
			templateSv := confSv.Url()
			codesSv := sv.Codes()
			lsm := map[string][]json.T{}
			nicks := nicksTb.Read().SelectedNicks()
			for _, nk := range nicks {
				var cdSv *server.CodeT
				for _, e := range codesSv {
					if e.NickId() == nk.Id() {
						cdSv = e
						break
					}
				}
				code := nk.Name()
				if cdSv != nil {
					ok := false
					code, ok = cdSv.Code()
					if !ok {
						code = nk.Name()
					}
				}
				url := strings.ReplaceAll(templateSv, "${code}", code)
				lsm[nk.Name()] = []json.T{json.Wb(false), json.Ws(url)}
			}

			for inv := 0; inv < cts.Qlevels; inv++ {
				anns := diariesDb.ReadAnnotations(inv)
				_, portfolio, _ := acc.Settlement(anns)
				for _, e := range portfolio {
					v, ok := lsm[e.Nick()]
					if ok {
						lsm[e.Nick()] = []json.T{json.Wb(true), v[1]}
					}
				}
			}

			var ls []json.T
			for k, v := range lsm {
				ls = append(ls, json.Wa([]json.T{json.Ws(k), v[0], v[1]}))
			}
			rp["list"] = json.Wa(ls)
		})
		return cgi.Rp(ck, rp)
	case "nickData":
		nkName := cgi.RqString(mrq, "nick")
		rp := map[string]json.T{}
		lock.Run(func() {
			var invs []json.T
			stocks := 0
			value := 0.0
			for inv := 0; inv < cts.Qlevels; inv++ {
				anns := diariesDb.ReadAnnotations(inv)
				_, portfolio, _ := acc.Settlement(anns)
				for _, e := range portfolio {
					if e.Nick() == nkName {
						invs = append(invs, json.Wi(inv))
						stocks += e.Stocks()
						value += float64(e.Stocks()) * e.Price()
					}
				}
			}
			rp["investors"] = json.Wa(invs)

			price := -1.0
			if stocks > 0 {
				price = value / float64(stocks)
			}
			rp["price"] = json.Wd(price)
			rp["profits"] = json.Wd(0)

			var nk *nick.T
			for _, e := range nicksTb.Read().List() {
				if e.Name() == nkName {
					nk = e
					break
				}
			}
			if nk == nil {
				rp["quotes"] = json.Wa([]json.T{})
				return
			}

			nickId := nk.Id()
			activity := confTb.Activity().Activity()
			closes := quotesDb.Closes()
			var cls [][]float64
			var ok bool
			if activity != cts.ActSleeping2 {
				var qv *nick.QvalueT
				for _, e := range dailyTb.Read() {
					if e.Nick() == nickId {
						qv = e
					}
				}
				if qv == nil {
					cls, ok = closes.NickValues(nkName)
				} else {
					cls, ok = closes.NickValuesAdd(nkName, qv.Value())
				}
			} else {
				cls, ok = closes.NickValues(nkName)
			}

			if !ok {
				rp["quotes"] = json.Wa([]json.T{})
				rp["dates"] = json.Wa([]json.T{})
				return
			}

			var datesJs []json.T
			for _, e := range quotesDb.Dates() {
				datesJs = append(datesJs, json.Ws(e))
			}
			rp["dates"] = json.Wa(datesJs)

			var quotes [][]json.T
			for _, ce := range cls {
				e := []json.T{json.Wd(ce[0])}
				for inv := 0; inv < cts.Qlevels; inv++ {
					e = append(e, json.Wd(-1.0))
				}
				quotes = append(quotes, e)
			}
			invsTb := investorsTb.Read()
			for inv := 0; inv < cts.Qlevels; inv++ {
				param := invsTb.Params[nkName][inv]
				md := model.New(inv, param)
				refs := md.Refs(cls)
				for iq := range quotes {
					quotes[iq][inv+1] = json.Wd(refs[iq])
				}
			}
			var qsjs []json.T
			for _, e := range quotes {
				qsjs = append(qsjs, json.Wa(e))
			}
			rp["quotes"] = json.Wa(qsjs)
			if stocks > 0 {
				rp["profits"] = json.Wd(
					float64(stocks) * (quotes[len(quotes)-1][0].Rd() - price),
				)
			}
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
