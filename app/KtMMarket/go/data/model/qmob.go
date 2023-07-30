// Copyright 04-Jul-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Model Mobil Quantum.
package model

import (
	"github.com/dedeme/KtMMarket/data/model/docs/qmobDoc"
)

func qmobCalc(
	closes [][]float64,
	params []float64,
	action func(closes, refs []float64),
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

	isSolds := make([]bool, nCos)
	refs := make([]float64, nCos)
	for i, c := range pvrow {
		refs[i] = c * (1 - gap)
	}

	for _, cs := range closes {
		newRefs := make([]float64, nCos)
		newIsSolds := make([]bool, nCos)

		for i, c := range cs {
			if c < 0 {
				newRefs[i] = refs[i]
				newIsSolds[i] = isSolds[i]
				continue
			}

			rf := refs[i]
			isSold := isSolds[i]

			if isSold {
				if c > rf {
					// newIsSolds[i] = false
					newRefs[i] = c * (1 - gap)
				} else {
					newIsSolds[i] = true
					newRef := c * (1 + gap)
					if newRef > rf {
						newRef = rf
					}
					newRefs[i] = newRef
				}
			} else {
				if c < rf {
					newIsSolds[i] = true
					newRefs[i] = c * (1 + gap)
				} else {
					// newIsSolds[i] = false
					newRef := c * (1 - gap)
					if newRef < rf {
						newRef = rf
					}
					newRefs[i] = newRef
				}
			}

		}
		refs = newRefs
		isSolds = newIsSolds

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
			0.01,
		},
		[]float64{
			0.005,
		},
		[]float64{
			0.001,
		},
		qmobCalc,
	}
}
