// Copyright 14-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// tests/Orders page
package orders

import (
	"github.com/dedeme/KtMarket/data/assetsRs"
	"github.com/dedeme/KtMarket/data/model"
	"github.com/dedeme/KtMarket/data/nick"
	"github.com/dedeme/KtMarket/data/order"
	"github.com/dedeme/KtMarket/data/quote"
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
	case "ordersData":
		modelId := js.Rs(mrq["modelId"])
		params := arr.Map(js.Ra(mrq["params"]), js.Rd)

		var ok bool
		var nks []string
		var lastCloses []float64
		var os []*order.T
		var assets *assetsRs.T

		thread.Sync(func() {
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
			arr.Sort(nks, func(n1, n2 string) bool {
				return n1 < n2
			})
			lg := len(nks)
			var opens [][]float64
			var closes [][]float64
			var maxs [][]float64
			var dates []string

			for i, nk := range nks {
				qs, err := db.QsRead(nk)
				if err != "" {
					log.Error(err)
					return
				}
				if i == 0 {
					dates = quote.Dates(qs)
					for range qs {
						opens = append(opens, make([]float64, lg))
						closes = append(closes, make([]float64, lg))
						maxs = append(maxs, make([]float64, lg))
					}
				}

				nkOpens := quote.Opens(qs)
				for j, o := range nkOpens {
					opens[j][i] = o
				}
				nkCloses := quote.Closes(qs)
				for j, c := range nkCloses {
					closes[j][i] = c
				}
				nkMaxs := quote.Maxs(qs)
				for j, m := range nkMaxs {
					maxs[j][i] = m
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

			var as []float64
			os, as, _, _, _, _, _, _ =
				strategy.Simulation(st, dates, nks, opens, closes, maxs)
			assets = assetsRs.New(as[len(as)-1], order.Buys(os), order.Sales(os))

			ok = true
		})

		return cgi.Rp(ck, cgi.T{
			"ok":         js.Wb(ok),
			"nicks":      js.Wa(arr.Map(nks, js.Ws)),
			"lastCloses": js.Wa(arr.Map(lastCloses, js.Wd)),
			"orders":     js.Wa(arr.Map(os, order.ToJs)),
			"assets":     assetsRs.ToJs(assets),
		})
	default:
		panic("Value of rq (" + rq + ") is not valid")
	}
}
