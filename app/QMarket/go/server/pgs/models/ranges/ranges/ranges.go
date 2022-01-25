// Copyright 14-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Ranges page.
package ranges

import (
	"fmt"
	"github.com/dedeme/QMarket/data/cts"
	"github.com/dedeme/QMarket/data/model"
	"github.com/dedeme/QMarket/db/confTb"
	"github.com/dedeme/QMarket/db/modelsDb"
	"github.com/dedeme/QMarket/db/quotesDb"
	"github.com/dedeme/QMarket/lock"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func values(qlevel int, start, end, step int) []json.T {
	var r []json.T

	subFirst := true
	subEnd := start
	subParam := 0
	n := 0
	sumEval := float64(0)
	sumSales := float64(0)
	fn := func(param int, rss []*model.RsT) bool {
		if param < start {
			return false
		}

		if param >= end {
			return true
		}

		rs := rss[qlevel]

		if subFirst {
			subParam = param
			subEnd = param + step
			subFirst = false
			n = 1
			sumEval = rs.HistoricValue()
			sumSales = rs.HistoricSales()
			return false
		}

		if param >= subEnd {
			r = append(r, json.Wa([]json.T{
				json.Wi(subParam),
				json.Wd(sumEval / float64(n)),
				json.Wd(sumSales / float64(n)),
			}))
			n = 1
			subParam = param
			subEnd = param + step
			sumEval = rs.HistoricValue()
			sumSales = rs.HistoricSales()
			return false
		}

		n++
		sumEval += rs.HistoricValue()
		sumSales += rs.HistoricSales()
		return false
	}

	modelsDb.EachResult(fn)

	if n > 0 {
		r = append(r, json.Wa([]json.T{
			json.Wi(subParam),
			json.Wd(sumEval / float64(n)),
			json.Wd(sumSales / float64(n)),
		}))
	}

	return r
}

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		qlevel := cgi.RqInt(mrq, "qlevel")
		rp := map[string]json.T{}
		lock.Run(func() {
			param := confTb.Range(qlevel)

			paramJs := []json.T{}
			for _, e := range param {
				paramJs = append(paramJs, json.Wi(e))
			}
			rp["param"] = json.Wa(paramJs)
			start := cts.RangesMin * cts.RangesGroupNumber
			end := (cts.RangesMin + cts.RangesGroups) * cts.RangesGroupNumber
			step := cts.RangesGroupNumber / 10
			if len(param) == 1 {
				start = param[0] * cts.RangesGroupNumber
				end = start + cts.RangesGroupNumber
				step = cts.RangesGroupNumber / 100
			} else if len(param) == 2 {
				start = param[0]*cts.RangesGroupNumber +
					param[1]*cts.RangesGroupNumber/10
				end = start + cts.RangesGroupNumber/10
				step = cts.RangesGroupNumber / 1000
			} else if len(param) == 3 {
				start = param[0]*cts.RangesGroupNumber +
					param[1]*cts.RangesGroupNumber/10 +
					param[2]*cts.RangesGroupNumber/100
				end = start + cts.RangesGroupNumber/100
				step = cts.RangesGroupNumber / 10000
			}
			rp["values"] = json.Wa(values(qlevel, start, end, step))
		})
		return cgi.Rp(ck, rp)
	case "changeParam":
		qlevel := cgi.RqInt(mrq, "qlevel")
		paramJs := mrq["param"]
		var param []int
		for _, p := range paramJs.Ra() {
			param = append(param, p.Ri())
		}
		lock.Run(func() {
			confTb.SetRange(qlevel, param)
		})
		return cgi.RpEmpty(ck)
	case "model":
		qlevel := cgi.RqInt(mrq, "qlevel")
		paramId := cgi.RqInt(mrq, "paramId")
		rp := map[string]json.T{}
		lock.Run(func() {
			opens := quotesDb.Opens()
			closes := quotesDb.Closes()
			md := model.New(qlevel, paramId)
			if ok, tb := modelsDb.Read(modelsDb.Group(md)); ok {
				if rs, ok := tb.Results()[paramId]; ok {
					md.SetEvaluation(rs[qlevel], opens, closes)
					rp["model"] = md.ToJs()
					return
				}
			}
			md.FirstEvaluation(opens, closes)
			rp["model"] = md.ToJs()
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
