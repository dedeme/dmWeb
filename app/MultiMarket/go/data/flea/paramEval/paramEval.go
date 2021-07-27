// Copyright 04-Jul-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Record of one param and its evaluation. Used in rangesPlus calculus.
package paramEval

import (
	"github.com/dedeme/MultiMarket/data/flea/eFlea"
	"sort"
)

type T struct {
	param float64
	eval  float64
	sales float64
}

type rangeMax struct {
	rg  []*T
	max *T
}

func New(param, eval, sales float64) *T {
	return &T{param, eval, sales}
}

func max(efleas []*T) *T {
	r := efleas[0]
	for _, e := range efleas {
		if e.eval > r.eval {
			r = e
		}
	}
	return r
}

func maxAvg(efleas []*T) (max *T, avg float64) {
	max = efleas[0]
	sum := 0.0
	for _, e := range efleas {
		if e.eval > max.eval {
			max = e
		}
		sum += e.eval
	}
	avg = sum / float64(len(efleas))
	return
}

func selection(parts int, efleas []*T) []*T {
	ln := len(efleas)
	if ln < parts {
		return []*T{max(efleas)}
	}

	n := ln / parts
	start := 0
	ix := 0
	mAvg := -10000.0
	var groups []*rangeMax

	for i := 0; i < parts-1; i++ {
		rg := efleas[start : start+n]
		start += n
		mx, avg := maxAvg(rg)
		if avg > mAvg {
			ix = i
			mAvg = avg
		}
		groups = append(groups, &rangeMax{rg, mx})
	}
	rg := efleas[start:]
	mx, avg := maxAvg(rg)
	if avg > mAvg {
		ix = parts - 1
	}
	groups = append(groups, &rangeMax{rg, mx})

	var r []*T
	for i, e := range groups {
		if i == ix {
			continue
		}
		r = append(r, e.max)
	}

	r = append(r, selection(parts, groups[ix].rg)...)

	return r
}

func paramEq(p1, p2 float64) bool {
	df := p2 - p1
	if df > 0.0000001 || df < -0.0000001 {
		return false
	}
	return true
}

type eFleas []*eFlea.T

func (a eFleas) Len() int      { return len(a) }
func (a eFleas) Swap(i, j int) { a[i], a[j] = a[j], a[i] }
func (a eFleas) Less(i, j int) bool {
	return a[i].HistoricEval > a[j].HistoricEval
}

func Ranking(
	efleas []*T, mkEval func(float64, float64, float64) *eFlea.T,
) []*eFlea.T {
	aparts := []int{2, 3, 5, 7, 11}
	var params []float64
	var r []*eFlea.T
	for _, parts := range aparts {
		for _, e := range selection(parts, efleas) {
			param := e.param
			add := true
			for _, i := range params {
				if paramEq(i, param) {
					add = false
					break
				}
			}

			if add {
				params = append(params, param)
				r = append(r, mkEval(param, e.eval, e.sales))
			}
		}
	}

	sort.Sort(eFleas(r))
	return r
}
