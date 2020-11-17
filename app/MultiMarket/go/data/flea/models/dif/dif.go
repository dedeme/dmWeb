// Copyright 10-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Difference investor model.
package dif

import (
	"github.com/dedeme/MultiMarket/data/flea/fmodel"
)

func fn(
	closes [][]float64, params []float64, action func([]float64, []float64),
) {
	nDays := len(closes)
	nCos := len(closes[0])
	difToBuy := params[0]
	difToSell := params[1]

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
		refs[iCo] = closes[ixDay][iCo] * (1.0 - difToSell)
	}

	for iDay := 0; iDay < nDays; iDay++ {
		for iCo := 0; iCo < nCos; iCo++ {
			q := closes[iDay][iCo]
			if q > 0 {
				ref := refs[iCo]
				if toBuys[iCo] {
          newRef := q * (1.0 + difToBuy)
          if newRef < ref {
            refs[iCo] = newRef
          } else if q > ref {
						refs[iCo] = q * (1.0 - difToSell)
						toBuys[iCo] = false
					}
				} else {
          newRef := q * (1.0 - difToSell)
          if newRef > ref {
            refs[iCo] = newRef
          } else if q < ref {
						refs[iCo] = q * (1.0 + difToBuy)
						toBuys[iCo] = true
					}
				}
			}
		}
		action(closes[iDay], refs)
	}
}

func Mk() *fmodel.T {
	return fmodel.New(
		"DIF",
		"Dif.",
		[]string{"Dif. C", "Dif. V"},
		[]float64{0.01, 0.01},
		[]float64{0.5, 0.5},
		[]int{6, 6},
		fn,
	)
}
