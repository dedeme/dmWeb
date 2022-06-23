// Copyright 14-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// model/companies page
package companies

import (
	"github.com/dedeme/KtMarket/data/model"
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

			arr.ReverseIn(qs)
			for _, q := range qs {
				dates = append(dates, q.Date)
				closes = append(closes, q.Close)
				opens = append(opens, q.Open)
			}
			st := strategy.New(md, params)
			refs = strategy.Refs(st, closes, -1)

			profits = strategy.Profits(st, opens, closes)

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