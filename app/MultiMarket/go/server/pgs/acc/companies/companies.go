// Copyright 19-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Companies accounting charts.
package companies

import (
	"fmt"
	"github.com/dedeme/MultiMarket/data/acc"
	"github.com/dedeme/MultiMarket/data/cts"
	"github.com/dedeme/MultiMarket/data/nick"
	"github.com/dedeme/MultiMarket/data/server"
	"github.com/dedeme/MultiMarket/db/acc/diariesDb"
	"github.com/dedeme/MultiMarket/db/conf"
	"github.com/dedeme/MultiMarket/db/dailyTb"
	"github.com/dedeme/MultiMarket/db/managersTb"
	"github.com/dedeme/MultiMarket/db/nicksTb"
	"github.com/dedeme/MultiMarket/db/quotesDb"
	"github.com/dedeme/MultiMarket/db/refsDb"
	"github.com/dedeme/MultiMarket/db/serversTb"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/date"
	"github.com/dedeme/golib/json"
	"strings"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "list":
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			servers := serversTb.Read(lk)
			_, selected := serversTb.HistoricList(lk)
			sv := servers[selected]
			confSv, _ := sv.HistoricConf()
			templateSv := confSv.Url()
			codesSv := sv.Codes()
			lsm := map[string][]json.T{}
			nicks := nicksTb.SelectedNicks(lk)
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

			for i := 0; i < cts.Managers; i++ {
				anns := diariesDb.ReadAnnotations(lk, i)
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
		sync.Run(func(lk sync.T) {
			stocks := 0
			value := 0.0
			for i := 0; i < cts.Managers; i++ {
				anns := diariesDb.ReadAnnotations(lk, i)
				_, portfolio, _ := acc.Settlement(anns)
				for _, e := range portfolio {
					if e.Nick() == nkName {
						stocks += e.Stocks()
						value += float64(e.Stocks()) * e.Price()
					}
				}
			}

			price := -1.0
			if stocks > 0 {
				price = value / float64(stocks)
			}
			rp["price"] = json.Wd(price)
			rp["profits"] = json.Wd(0)

			var nk *nick.T
			for _, e := range nicksTb.Nicks(lk) {
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
			activity := conf.Activity(lk).Activity()
			dates := quotesDb.Dates(lk)
			closes := quotesDb.Closes(lk)
			var cls [][]float64
			var ok bool
			if activity != cts.ActSleeping2 {
				var qv *nick.QvalueT
				for _, e := range dailyTb.Read(lk) {
					if e.Nick == nickId {
						qv = e
					}
				}
				if qv == nil {
					cls, ok = closes.NickValues(nkName)
				} else {
					cls, ok = closes.NickValuesAdd(nkName, qv.Value)
					dates = append(dates, date.Now().String())[1:]
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
			for _, e := range dates {
				datesJs = append(datesJs, json.Ws(e))
			}
			rp["dates"] = json.Wa(datesJs)

			var quotes [][]json.T
			for _, ce := range cls {
				e := []json.T{json.Wd(ce[0])}
				for i := 0; i < cts.Managers; i++ {
					e = append(e, json.Wd(-1.0))
				}
				quotes = append(quotes, e)
			}
			for i, man := range managersTb.Read(lk) {
				mdPars := man.GetModel(nkName)
				refs := refsDb.MkRefs(
					lk, nkName, dates, cls, mdPars.Model(), mdPars.Params(),
				)
				for iq := range quotes {
					quotes[iq][i+1] = json.Wd(refs[iq])
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
