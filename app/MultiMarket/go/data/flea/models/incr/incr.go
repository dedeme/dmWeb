// Copyright 10-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Incrememt investor model.
package incr

import (
	"github.com/dedeme/MultiMarket/data/flea/fmodel"
)

func fn(
	closes [][]float64, params []float64, action func([]float64, []float64),
) {
	nDays := len(closes)
	nCos := len(closes[0])
	days := int(params[0])
	stripToBuy := params[1]
	stripToSell := params[2]

	toBuys := make([]bool, nCos)
	refs := make([]float64, nCos)
	refs2 := make([]float64, nCos)
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
		refs[iCo] = closes[ixDay][iCo]
	}

	for iDay := 0; iDay < nDays; iDay++ {
		for iCo := 0; iCo < nCos; iCo++ {
			if iDay < days {
				refs2[iCo] = -1
			} else {
				q := closes[iDay][iCo]
				if q > 0 {
					ref := refs[iCo]
					newRef := closes[iDay-days][iCo]
					if newRef < 0 {
						newRef = ref
					}
					if toBuys[iCo] {
						if ref > newRef {
							ref = newRef
						}
						refs[iCo] = ref
						ref2 := ref * (1 + stripToBuy)
						refs2[iCo] = ref2
						if q > ref2 {
							toBuys[iCo] = false
							refs2[iCo] = ref * (1 - stripToSell)
						}
					} else {
						if ref < newRef {
							ref = newRef
						}
						refs[iCo] = ref
						ref2 := ref * (1 - stripToSell)
						refs2[iCo] = ref2
						if q < ref2 {
							toBuys[iCo] = true
							refs2[iCo] = ref * (1 + stripToBuy)
						}
					}
				}
			}
		}
		action(closes[iDay], refs2)
	}
}

func Mk() *fmodel.T {
	return fmodel.New(
		"INCR",
		"Incremento",
		[]string{"Días", "Banda C", "Banda V"},
		[]float64{20, 0.0001, 0.0001},
		[]float64{120, 0.25, 0.25},
		[]int{0, 6, 6},
		fn,
	)
}
