// Copyright 10-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Approx investor model.
package appr

import (
	"github.com/dedeme/MultiMarket/data/flea/fmodel"
)

func fn(
	closes [][]float64, params []float64, action func([]float64, []float64),
) {
	nDays := len(closes)
	nCos := len(closes[0])
	startToBuy := params[0]
	stepToBuy := params[1]
	startToSell := params[2]
	stepToSell := params[3]

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
		refs[iCo] = closes[ixDay][iCo] * (1.0 - startToSell)
	}

	for iDay := 0; iDay < nDays; iDay++ {
		for iCo := 0; iCo < nCos; iCo++ {
			q := closes[iDay][iCo]
			if q > 0 {
				ref := refs[iCo]
				if toBuys[iCo] {
					ref -= (ref - q) * stepToBuy
					if q > ref {
						refs[iCo] = q * (1.0 - startToSell)
						toBuys[iCo] = false
					} else {
						refs[iCo] = ref
					}
				} else {
					ref += (q - ref) * stepToSell
					if q < ref {
						refs[iCo] = q * (1.0 + startToBuy)
						toBuys[iCo] = true
					} else {
						refs[iCo] = ref
					}
				}
			}
		}
		action(closes[iDay], refs)
	}
}

func Mk() *fmodel.T {
	return fmodel.New(
		"APPR",
		"Approx.",
		[]string{"Inicio C", "Paso C", "Inicio V", "Paso V"},
		[]float64{0.05, 0.01, 0.05, 0.01},
		[]float64{0.3, 0.15, 0.3, 0.15},
		[]int{6, 6, 6, 6},
		fn,
	)
}
