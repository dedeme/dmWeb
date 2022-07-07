// Copyright 04-Jul-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Company charts page.
package cosPg

import (
	"github.com/dedeme/KtMMarket/data/model"
	"github.com/dedeme/KtMMarket/data/modelEval"
	"github.com/dedeme/KtMMarket/data/order"
	"github.com/dedeme/KtMMarket/data/result"
	"github.com/dedeme/KtMMarket/db"
	"github.com/dedeme/KtMMarket/fns"
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
)

func Process(ck string, mrq cgi.T) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "idata":
		modelId := js.Rs(mrq["modelId"])
		paramsJs := mrq["params"]
		md := model.FromId(modelId)
		evs := db.EvalsDb(modelId).Read().Evals

		var params []float64
		var eval *modelEval.T
		if !js.IsNull(paramsJs) {
			params = arr.Map(js.Ra(paramsJs), js.Rd)
			var ok bool
			eval, ok = arr.Find(evs, func(ev *modelEval.T) bool {
				return fns.EqParams(ev.Params, params)
			})
			if !ok {
				paramsJs = js.Wn()
			}
		}
		if js.IsNull(paramsJs) { // Warning: Not use 'else'
			arr.Sort(evs, func(e1, e2 *modelEval.T) bool {
				return e1.Hvalue > e2.Hvalue
			})
			eval = evs[0]
			params = eval.Params
		}

		qs := db.QuotesTb().Read()
		orders, historic, _, _, _, _, _, profits := md.Simulation(qs, params)
		rs := result.New(
			historic[len(historic)-1],
			arr.Reduce(profits, 0, func(r, e float64) float64 {
				return r + e
			})/float64(len(profits)),
			float64(order.Sales(orders)),
		)

		return cgi.Rp(ck, cgi.T{
			"model":  model.ToJs(md),
			"result": result.ToJs(rs),
			"eval":   modelEval.ToJs(eval),
			"cos":    js.Wa(arr.Map(qs.Cos, js.Ws)),
		})

	case "co":
		modelId := js.Rs(mrq["modelId"])
		params := arr.Map(js.Ra(mrq["params"]), js.Rd)
		co := js.Rs(mrq["co"])
		md := model.FromId(modelId)
		allQs := db.QuotesTb().Read()
		coIx := allQs.CompanyIndex(co)
		if coIx == -1 {
			panic("Company " + co + " not found")
		}

		qs := allQs.GetSingle(coIx)
		orders, historic, _, _, refs, _, _, profits := md.Simulation(qs, params)
		rs := result.New(
			historic[len(historic)-1],
			arr.Reduce(profits, 0, func(r, e float64) float64 {
				return r + e
			})/float64(len(profits)),
			float64(order.Sales(orders)),
		)

		return cgi.Rp(ck, cgi.T{
			"result": result.ToJs(rs),
			"dates":  js.Wa(arr.Map(qs.Dates, js.Ws)),
			"qs": js.Wa(arr.Map(qs.Closes, func(c []float64) string {
				return js.Wd(c[0])
			})),
			"refs": js.Wa(arr.Map(refs, func(r []float64) string {
				return js.Wd(r[0])
			})),
		})

	default:
		panic("Value of rq (" + rq + ") is not valid")
	}
}
