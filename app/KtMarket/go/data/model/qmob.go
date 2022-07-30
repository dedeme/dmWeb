// Copyright 14-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Model Mobil Quantum.
package model

import (
	"github.com/dedeme/KtMarket/data/model/docs/qmobDoc"
	"github.com/dedeme/KtMarket/data/reference"
)

func qmobCalc(
	closes [][]float64,
	params []float64,
	refs []*reference.T,
	action func(closes []float64, refs []*reference.T),
) {
	gap := params[0]
	nCos := len(closes[0])
	pvrow := make([]float64, nCos)
	for i := 0; i < nCos; i++ {
		for _, cs := range closes {
			if cs[i] > 0 {
				pvrow[i] = cs[i]
				break
			}
		}
		if pvrow[i] == 0 {
			pvrow[i] = 1
		}
	}

	for i, c := range pvrow {
		if refs[i].Ref < 0 {
			refs[i] = reference.New(c*(1-gap), true)
		}
	}

	for _, cs := range closes {
		newRefs := make([]*reference.T, nCos)

		for i, c := range cs {
			rf := refs[i]

			if rf.InPortfolio {
				if c < rf.Ref {
					newRefs[i] = reference.New(c*(1+gap), false)
				} else {
					newRef := c * (1 - gap)
					if newRef < rf.Ref {
						newRef = rf.Ref
					}
					newRefs[i] = reference.New(newRef, true)
				}
			} else {
				if c > rf.Ref {
					newRefs[i] = reference.New(c*(1-gap), true)
				} else {
					newRef := c * (1 + gap)
					if newRef > rf.Ref {
						newRef = rf.Ref
					}
					newRefs[i] = reference.New(newRef, false)
				}
			}

		}
		refs = newRefs

		action(cs, refs)
	}

}

func newQmob() *T {
	return &T{
		"QMOV",
		"Quantum móvil",
		qmobDoc.Get(),
		[]string{
			"Intervalo",
		},
		[]float64{
			0.205,
		},
		[]float64{
			0.110,
		},
		[]int{
			4,
		},
		qmobCalc,
	}
}
