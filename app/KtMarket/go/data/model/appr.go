// Copyright 14-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Model Approximations.
package model

import (
	"github.com/dedeme/KtMarket/data/model/docs/apprDoc"
	"github.com/dedeme/KtMarket/data/reference"
)

func apprCalc(
	closes [][]float64,
	params []float64,
	refs []*reference.T,
	action func(closes []float64, refs []*reference.T),
) {
	start := params[0]
	incr := params[1]
	nCos := len(closes[0])

	for i, c := range closes[0] {
		if refs[i].Ref < 0 {
			refs[i] = reference.New(c*(1-start), true)
		}
	}

	for _, cs := range closes {
		newRefs := make([]*reference.T, nCos)

		for i, c := range cs {
			rf := refs[i]

			if rf.InPortfolio {
				if c < rf.Ref {
					newRefs[i] = reference.New(c*(1+start), false)
				} else {
					newRefs[i] = reference.New(rf.Ref+(c-rf.Ref)*incr, true)
				}
			} else {
				if c > rf.Ref {
					newRefs[i] = reference.New(c*(1-start), true)
				} else {
					newRefs[i] = reference.New(rf.Ref-(rf.Ref-c)*incr, false)
				}
			}

		}
		refs = newRefs

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
