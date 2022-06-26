// Copyright 14-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Models page.
package modelsPg

import (
	"github.com/dedeme/KtMarket/data/assetsRs"
	"github.com/dedeme/KtMarket/data/investor"
	"github.com/dedeme/KtMarket/data/model"
	"github.com/dedeme/KtMarket/data/nick"
	"github.com/dedeme/KtMarket/data/order"
	"github.com/dedeme/KtMarket/data/strategy"
	"github.com/dedeme/KtMarket/db"
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/log"
	"github.com/dedeme/ktlib/thread"
)

func Process(ck string, mrq cgi.T) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "idata":
		return cgi.Rp(ck, cgi.T{
			"models": js.Wa(arr.Map(model.List(), model.ToJs)),
		})
	case "results":
		modelId := js.Rs(mrq["modelId"])
		paramsJs := mrq["params"]

		var ok bool
		var params []float64
		var dates []string
		var nks []string
		var lastCloses []float64
		var os []*order.T
		var results *assetsRs.T
		var assets []float64

		thread.Sync(func() {
			inv, ok2 := arr.Find(
				db.InvestorsTb().Read().Investors, func(inv *investor.T) bool {
					return inv.Base.Model.Id == modelId
				})
			if !ok2 {
				log.Error("Investor with model " + modelId + " not found")
				inv = db.InvestorsTb().Read().Investors[0]
			}
			params = inv.Base.Params
			if !js.IsNull(paramsJs) {
				params = arr.Map(js.Ra(mrq["params"]), js.Rd)
			}

			err := ""
			dates, err = db.Dates()
			if err != "" {
				log.Error(err)
				return
			}

			md, ok2 := arr.Find(model.List(), func(m *model.T) bool {
				return m.Id == modelId
			})
			if !ok2 {
				log.Error("Model '" + modelId + "' not found")
				return
			}
			st := strategy.New(md, params)

			nks = arr.Map(arr.Filter(db.NicksTb().Read().List, func(n *nick.T) bool {
				return n.IsSel
			}),
				func(n *nick.T) string {
					return n.Name
				})
			lg := len(nks)
			var opens [][]float64
			var closes [][]float64

			for i, nk := range nks {
				qs, err := db.QsRead(nk)
				if err != "" {
					log.Error(err)
					return
				}
				arr.ReverseIn(qs)

				if i == 0 {
					for range qs {
						opens = append(opens, make([]float64, lg))
						closes = append(closes, make([]float64, lg))
					}
				}

				for j, q := range qs {
					opens[j][i] = q.Open
					closes[j][i] = q.Close
				}
			}

			for i := 0; i < lg; i++ {
				lc := 0.0
				for j := len(closes) - 1; j >= 0; j-- {
					c := closes[j][i]
					if c >= 0 {
						lc = c
						break
					}
				}
				lastCloses = append(lastCloses, lc)
			}

			os, results = strategy.Orders(st, dates, nks, opens, closes)
			assets = strategy.Assets(st, opens, closes)

			ok = true
		})

		return cgi.Rp(ck, cgi.T{
			"ok":         js.Wb(ok),
			"params":     js.Wa(arr.Map(params, js.Wd)),
			"dates":      js.Wa(arr.Map(dates, js.Ws)),
			"assets":     js.Wa(arr.Map(assets, js.Wd)),
			"results":    assetsRs.ToJs(results),
			"nicks":      js.Wa(arr.Map(nks, js.Ws)),
			"lastCloses": js.Wa(arr.Map(lastCloses, js.Wd)),
			"orders":     js.Wa(arr.Map(os, order.ToJs)),
		})
	default:
		panic("Value of rq (" + rq + ") is not valid")
	}
}
