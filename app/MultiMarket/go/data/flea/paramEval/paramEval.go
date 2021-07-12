// Copyright 04-Jul-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Record of one param and its evaluation. Used in rangesPlus calculus.
package paramEval

import (
	"github.com/dedeme/MultiMarket/data/flea/eval"
	"sort"
)

type T struct {
	param float64
	eval  float64
	sales int
}

type rangeMax struct {
	rg  []*T
	max *T
}

func New(param, eval float64, sales int) *T {
	return &T{param, eval, sales}
}

func max(pevals []*T) *T {
	r := pevals[0]
	for _, e := range pevals {
		if e.eval > r.eval {
			r = e
		}
	}
	return r
}

func maxAvg(pevals []*T) (max *T, avg float64) {
	max = pevals[0]
	sum := 0.0
	for _, e := range pevals {
		if e.eval > max.eval {
			max = e
		}
		sum += e.eval
	}
	avg = sum / float64(len(pevals))
	return
}

func selection(parts int, pevals []*T) []*T {
	ln := len(pevals)
	if ln < parts {
		return []*T{max(pevals)}
	}

	n := ln / parts
	start := 0
	ix := 0
	mAvg := -10000.0
	var groups []*rangeMax

	for i := 0; i < parts-1; i++ {
		rg := pevals[start : start+n]
		start += n
		mx, avg := maxAvg(rg)
		if avg > mAvg {
			ix = i
			mAvg = avg
		}
		groups = append(groups, &rangeMax{rg, mx})
	}
	rg := pevals[start:]
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

type evals []*eval.T

func (a evals) Len() int           { return len(a) }
func (a evals) Swap(i, j int)      { a[i], a[j] = a[j], a[i] }
func (a evals) Less(i, j int) bool { return a[i].Eval > a[j].Eval }

func Ranking(
	pevals []*T, mkEval func(float64, float64, int) *eval.T,
) []*eval.T {
	aparts := []int{2, 3, 5, 7, 11}
	var params []float64
	var r []*eval.T
	for _, parts := range aparts {
		for _, e := range selection(parts, pevals) {
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

	sort.Sort(evals(r))
	return r
}
