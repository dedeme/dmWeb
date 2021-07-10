// Copyright 15-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Ranges page.
package ranges

import (
	"fmt"
	"github.com/dedeme/MultiMarket/data/flea/fmodels"
	"github.com/dedeme/MultiMarket/db/fleas/rangesTb"
	"github.com/dedeme/MultiMarket/db/fleas/resultsDb"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func values(lk sync.T, modelId string, start, end, step float64) []json.T {
	var r []json.T

	subFirst := true
	subEnd := start
	subParam := float64(0)
	n := 0
	sumEval := float64(0)
	sumSales := 0
	fn := func(param, eval float64, sales int, lastE float64, lastS int) bool {
		if param < start {
			return false
		}

		if param >= end {
			return true
		}

		if subFirst {
			subParam = param
			subEnd = param + step
			subFirst = false
			n = 1
			sumEval = eval
			sumSales = sales
			return false
		}

		if param >= subEnd {
			r = append(r, json.Wa([]json.T{
				json.Wd(subParam),
				json.Wd(sumEval / float64(n)),
				json.Wi(sumSales / n),
			}))
			n = 1
			subParam = param
			subEnd = param + step
			sumEval = eval
			sumSales = sales
			return false
		}

		n++
		sumEval += eval
		sumSales += sales
		return false
	}

	resultsDb.EachResult(lk, modelId, fn)

	if n > 0 {
		r = append(r, json.Wa([]json.T{
			json.Wd(subParam),
			json.Wd(sumEval / float64(n)),
			json.Wi(sumSales / n),
		}))
	}

	return r
}

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		modelId := cgi.RqString(mrq, "modelId")
		models := fmodels.List()
		var modelIds []string
		for _, m := range models {
			modelIds = append(modelIds, m.Id())
		}
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			rangesTb.Clean(lk, modelIds)
			param := rangesTb.ReadModel(lk, modelId)

			paramJs := []json.T{}
			for _, e := range param {
				paramJs = append(paramJs, json.Wi(e))
			}
			rp["param"] = json.Wa(paramJs)
			start := float64(0)
			end := float64(1.00)
			step := float64(0.001)
			if len(param) == 1 {
				start = float64(param[0]) / 100
				end = float64(param[0]+1) / 100
				step = float64(0.0001)
			} else if len(param) == 2 {
				start = float64(param[0])/100 + float64(param[1])/1000
				end = float64(param[0])/100 + float64(param[1]+1)/1000
				step = float64(0.00001)
			} else if len(param) == 3 {
				start = float64(param[0])/100 +
					float64(param[1])/1000 +
					float64(param[2])/10000
				end = float64(param[0])/100 +
					float64(param[1])/1000 +
					float64(param[2]+1)/10000
				step = float64(0.000001)
			}
			rp["values"] = json.Wa(values(lk, modelId, start, end, step))
		})
		return cgi.Rp(ck, rp)
	case "changeParam":
		modelId := cgi.RqString(mrq, "modelId")
		paramJs := mrq["param"]
		var param []int
		for _, p := range paramJs.Ra() {
			param = append(param, p.Ri())
		}
		sync.Run(func(lk sync.T) {
			rangesTb.Write(lk, modelId, param)
		})
		return cgi.RpEmpty(ck)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
