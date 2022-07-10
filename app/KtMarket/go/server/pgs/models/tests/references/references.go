// Copyright 14-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// tests/References page
package references

import (
	"github.com/dedeme/KtMarket/data/model"
	"github.com/dedeme/KtMarket/data/nick"
	"github.com/dedeme/KtMarket/data/quote"
	"github.com/dedeme/KtMarket/data/strategy"
	"github.com/dedeme/KtMarket/db"
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/log"
	"github.com/dedeme/ktlib/thread"
)

func mkArray(n float64) []float64 {
	return []float64{n}
}

func Process(ck string, mrq cgi.T) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "nickList":
		var lst string
		thread.Sync(func() {
			lst = js.Wa(arr.Map(arr.Filter(
				db.NicksTb().Read().List,
				func(n *nick.T) bool {
					return n.IsSel
				},
			),
				func(n *nick.T) string {
					return js.Ws(n.Name)
				},
			))
		})
		return cgi.Rp(ck, cgi.T{
			"nickList": lst,
		})
	case "chartData":
		modelId := js.Rs(mrq["modelId"])
		nickName := js.Rs(mrq["nickName"])
		params := arr.Map(js.Ra(mrq["params"]), js.Rd)

		var ok bool
		var dates []string
		var closes []float64
		var opens []float64
		var refs []float64
		var profits float64

		thread.Sync(func() {
			md, ok2 := arr.Find(model.List(), func(m *model.T) bool {
				return m.Id == modelId
			})
			if !ok2 {
				log.Error("Model '" + modelId + "' not found")
				return
			}

			qs, err := db.QsRead(nickName)
			if err != "" {
				log.Error(err)
				return
			}

			dates = quote.Dates(qs)
			closes = quote.Closes(qs)
			opens = quote.Opens(qs)
			maxs := quote.Maxs(qs)
			st := strategy.New(md, params)
			refs = strategy.Refs(st, closes, -1)

			_, _, _, _, _, _, _, profitss :=
				strategy.Simulation(st, dates, []string{nickName},
					arr.Map(opens, mkArray),
					arr.Map(closes, mkArray),
					arr.Map(maxs, mkArray),
				)
			profits = profitss[0]

			ok = true
		})

		return cgi.Rp(ck, cgi.T{
			"ok": js.Wb(ok),
			"dates": js.Wa(arr.Map(dates, func(e string) string {
				return js.Ws(e)
			})),
			"closes": js.Wa(arr.Map(closes, func(e float64) string {
				return js.Wd(e)
			})),
			"opens": js.Wa(arr.Map(opens, func(e float64) string {
				return js.Wd(e)
			})),
			"refs": js.Wa(arr.Map(refs, func(e float64) string {
				return js.Wd(e)
			})),
			"profits": js.Wd(profits),
		})

	default:
		panic("Value of rq (" + rq + ") is not valid")
	}
}
