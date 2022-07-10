// Copyright 14-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Model Approximations.
package model

import (
	"github.com/dedeme/KtMarket/data/model/docs/apprDoc"
)

func apprCalc(
	closes [][]float64,
	params []float64,
	refs []float64,
	action func(closes, refs []float64),
) {
	start := params[0]
	incr := params[1]
	nCos := len(closes[0])

	isSolds := make([]bool, nCos)
	for i, c := range closes[0] {
		rf := refs[i]
		if rf < 0 {
			refs[i] = c * (1 - start)
		} else {
			if rf > c {
				isSolds[i] = true
			}
		}
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
					newRefs[i] = rf - (rf-c)*incr
				}
			} else {
				if c < rf {
					newIsSolds[i] = true
					newRefs[i] = c * (1 + start)
				} else {
					// newIsSolds[i] = false
					newRefs[i] = rf + (c-rf)*incr
				}
			}

		}
		refs = newRefs
		isSolds = newIsSolds

		action(cs, refs)
	}

}

func newAppr() *T {
	return &T{
		"APRX",
		"Aproximaciones sucesivas",
		apprDoc.Get(),
		[]string{
			"Inicio",
			"Aproximación",
		},
		[]float64{
			0.42,
			0.038,
		},
		[]float64{
			0.04,
			0.002,
		},
		[]int{
			4,
			4,
		},
		apprCalc,
	}
}
