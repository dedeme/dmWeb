// Copyright 01-Jul-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Model Approximations.
package model

import (
	"github.com/dedeme/KtMMarket/data/model/docs/appr2Doc"
)

func appr2Calc(
	closes [][]float64,
	params []float64,
	action func(closes, refs []float64),
) {
	start := params[0]
	incr := params[1]
	nCos := len(closes[0])

	isSolds := make([]bool, nCos)
	refs := make([]float64, nCos)
	for i, c := range closes[0] {
		refs[i] = c * (1 - start)
	}

	for _, cs := range closes {
		newRefs := make([]float64, nCos)
		newIsSolds := make([]bool, nCos)

		for i, c := range cs {
			rf := refs[i]
			isSold := isSolds[i]

			if isSold {
				if c > rf {
					// newIsSolds[i] = false
					newRefs[i] = c * (1 - start)
				} else {
					newIsSolds[i] = true
					r1 := rf - (rf-c)*incr
					r2 := c * (1 + start)
					if r1 < r2 {
						newRefs[i] = r1
					} else {
						newRefs[i] = r2
					}
				}
			} else {
				if c < rf {
					newIsSolds[i] = true
					newRefs[i] = c * (1 + start)
				} else {
					// newIsSolds[i] = false
					r1 := rf + (c-rf)*incr
					r2 := c * (1 - start)
					if r1 > r2 {
						newRefs[i] = r1
					} else {
						newRefs[i] = r2
					}
				}
			}

		}
		refs = newRefs
		isSolds = newIsSolds

		action(cs, refs)
	}

}

func newAppr2() *T {
	return &T{
		"APRX2",
		"Aproximaciones sucesivas (2)",
		appr2Doc.Get(),
		[]string{
			"Inicio",
			"Aproximación",
		},
		[]float64{
			0.01,
			0.006,
		},
		[]float64{
			0.01,
			0.006,
		},
		[]float64{
			0.002,
			0.001,
		},
		appr2Calc,
	}
}
