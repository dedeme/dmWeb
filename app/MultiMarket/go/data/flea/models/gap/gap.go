// Copyright 10-Mar-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Gap investor model.
package gap

import (
	"github.com/dedeme/MultiMarket/data/flea/fmodel"
	"github.com/dedeme/MultiMarket/data/flea/refPos"
)

func fn(
	closes [][]float64, params []float64, init *refPos.T,
	action func([]float64, []float64),
) {
	nDays := len(closes)
	nCos := len(closes[0])
	pgap := params[0]

	toBuys := make([]bool, nCos)
	refs := make([]float64, nCos)
	for iCo := 0; iCo < nCos; iCo++ {
		ixDay := -1
		for iDay := 0; iDay < nDays; iDay++ {
			if closes[iDay][iCo] > 0 {
				ixDay = iDay
				break
			}
		}
		if ixDay < 0 {
			panic("iDay < 0")
		}
		q := closes[ixDay][iCo]
		refs[iCo] = q * (1 - pgap)
	}

	for iDay := 0; iDay < nDays; iDay++ {
		for iCo := 0; iCo < nCos; iCo++ {
			q := closes[iDay][iCo]
			if q > 0 {
				ref := refs[iCo]
				if toBuys[iCo] {
					if q > ref {
						refs[iCo] = q * (1 - pgap)
						toBuys[iCo] = false
					} else {
						newRef := q * (1 + pgap)
						if newRef < ref {
							refs[iCo] = newRef
						}
					}
				} else {
					if q < ref {
						refs[iCo] = q * (1 + pgap)
						toBuys[iCo] = true
					} else {
						newRef := q * (1 - pgap)
						if newRef > ref {
							refs[iCo] = newRef
						}
					}
				}
			}
		}
		action(closes[iDay], refs)
	}
}

func Mk() *fmodel.T {
	return fmodel.New(
		"GAP",
		"Gap",
		[]string{"Hueco"},
		[]float64{0.01},
		[]float64{0.25},
		[]int{6},
		true,
		fn,
	)
}
