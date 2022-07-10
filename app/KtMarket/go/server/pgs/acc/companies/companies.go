// Copyright 19-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// acc/companies page
package companies

import (
	"github.com/dedeme/KtMarket/cts"
	"github.com/dedeme/KtMarket/data/acc"
	"github.com/dedeme/KtMarket/data/nick"
	"github.com/dedeme/KtMarket/data/refBase"
	"github.com/dedeme/KtMarket/data/server"
	"github.com/dedeme/KtMarket/data/strategy"
	"github.com/dedeme/KtMarket/db"
	"github.com/dedeme/KtMarket/db/acc/diariesDb"
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/log"
	"github.com/dedeme/ktlib/str"
	"github.com/dedeme/ktlib/thread"
)

func Process(ck string, mrq cgi.T) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "list":

		var ok bool
		lst := map[string][]string{}

		thread.Sync(func() {
			nks := arr.Filter(db.NicksTb().Read().List, func(nk *nick.T) bool {
				return nk.IsSel
			})
			svs, selected := db.ServersTb().Read().HistoricList()
			sv := svs[selected]
			confSv, _ := sv.HistoricConf()
			for _, nk := range nks {
				nkCd := nk.Name
				cd, ok2 := arr.Find(sv.Codes, func(c *server.CodeT) bool {
					return c.NickId == nk.Id
				})
				if ok2 {
					nkCd2, ok2 := cd.Code()
					if ok2 {
						nkCd = nkCd2
					}
				}

				lst[nk.Name] = []string{
					js.Wb(false),
					js.Ws(str.Replace(confSv.Url, "${code}", nkCd)),
				}
			}

			for i := 0; i < cts.Investors; i++ {
				anns := diariesDb.ReadAnnotations(i)
				_, portfolio, _, errs := acc.Settlement(anns)
				if len(errs) != 0 {
					for _, e := range errs {
						log.Error(e)
					}
					return
				}

				for _, e := range portfolio {
					v, ok2 := lst[e.Nick]
					if !ok2 {
						log.Error("Unknown portfolio nick " + e.Nick)
						return
					}
					lst[e.Nick] = []string{js.Wb(true), v[1]}
				}
			}

			ok = true
		})

		var alst []string
		for k, v := range lst {
			alst = append(alst, js.Wa([]string{js.Ws(k), v[0], v[1]}))
		}

		return cgi.Rp(ck, cgi.T{
			"ok":   js.Wb(ok),
			"list": js.Wa(alst),
		})

	case "nickData":

		nkName := js.Rs(mrq["nick"])

		var dates []string
		var quotes [][]float64
		var investors []int
		price := -1.0
		var profits float64

		thread.Sync(func() {
			refBases := db.RefBasesTb().Read().Refs
			nk, ok := arr.Find(db.NicksTb().Read().List, func(nk *nick.T) bool {
				return nk.Name == nkName
			})
			if !ok {
				log.Error("Nick " + nkName + " not found")
				return
			}
			var err string
			var cls []float64
			dates, cls, err = db.CurrentCloses(nk)
			if err != "" {
				log.Error(err)
				dates = []string{}
				return
			}
			dates = arr.Drop(dates, len(dates)-cts.ReferenceQuotes)
			cls = arr.Drop(cls, len(cls)-cts.ReferenceQuotes)

			quotes = make([][]float64, len(cls))
			for i, c := range cls {
				qs := make([]float64, cts.Investors+1)
				qs[0] = c
				quotes[i] = qs
			}

			invList := db.InvestorsTb().Read().Investors
			stocks := 0
			for i := 0; i < cts.Investors; i++ {
				inv := invList[i]
				st := inv.Base
				st2, ok := inv.Nicks[nkName]
				if ok {
					st = st2
				}

				ref := -1.0
				initRef, ok := arr.Find(refBases, func(r *refBase.T) bool {
					return r.Nick.Name == nkName && strategy.Eq(r.Strategy, st)
				})
				if ok {
					ref = initRef.Ref
				}

				for j, r := range strategy.Refs(st, cls, ref) {
					quotes[j][i+1] = r
				}
				anns := diariesDb.ReadAnnotations(i)
				_, portfolio, _, es := acc.Settlement(anns)
				if len(es) > 0 {
					for _, e := range es {
						log.Error(e)
					}
					return
				}

				pfEntry, ok := arr.Find(portfolio, func(e *acc.PfEntryT) bool {
					return e.Nick == nkName
				})
				if ok {
					investors = append(investors, i)
					ttPrice := float64(pfEntry.Stocks) * pfEntry.Price
					sum := stocks + pfEntry.Stocks
					price = (price*float64(stocks) + ttPrice) / float64(sum)
					stocks = sum
				}
			}
			if stocks > 0 {
				profits = float64(stocks) * (cls[len(cls)-1] - price)
			}
		})

		return cgi.Rp(ck, cgi.T{
			"investorsN": js.Wi(cts.Investors),
			"dates":      js.Wa(arr.Map(dates, js.Ws)),
			"quotes": js.Wa(arr.Map(quotes, func(vs []float64) string {
				return js.Wa(arr.Map(vs, js.Wd))
			})),
			"investors": js.Wa(arr.Map(investors, js.Wi)),
			"price":     js.Wd(price),
			"profits":   js.Wd(profits),
		})

	default:
		panic("Value of rq (" + rq + ") is not valid")
	}
}
