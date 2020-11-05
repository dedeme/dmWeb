// Copyright 16-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Maximum-Minimum investor model.
package mm

import (
	"github.com/dedeme/MultiMarket/data/flea/fmodel"
)

func fn(
	closes [][]float64, params []float64, action func([]float64, []float64),
) {
	nDays := len(closes)
	nCos := len(closes[0])
	stripToBuy := params[0]
	stepToBuy := params[1]
	stripToSell := params[2]
	stepToSell := params[3]

	toBuys := make([]bool, nCos)
	mms := make([]float64, nCos)
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
		mms[iCo] = closes[ixDay][iCo]
		refs[iCo] = closes[ixDay][iCo] * (1.0 - stripToSell)
	}

	for iDay := 0; iDay < nDays; iDay++ {
		for iCo := 0; iCo < nCos; iCo++ {
			q := closes[iDay][iCo]
			if q > 0 {
				ref := refs[iCo]
				if toBuys[iCo] {
					ref -= (ref - q) * stepToBuy
					if q > ref {
						refs[iCo] = mms[iCo] * (1.0 - stripToSell)
						mms[iCo] = q
						toBuys[iCo] = false
					} else {
						refs[iCo] = ref
						if q < mms[iCo] {
							mms[iCo] = q
						}
					}
				} else {
					ref += (q - ref) * stepToSell
					if q < ref {
						refs[iCo] = mms[iCo] * (1.0 + stripToBuy)
						mms[iCo] = q
						toBuys[iCo] = true
					} else {
						refs[iCo] = ref
						if q > mms[iCo] {
							mms[iCo] = q
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
		"MM",
		"MaxMin",
		[]string{"Banda C", "Paso C", "Banda V", "Paso V"},
		[]float64{0.00, 0.001, 0.00, 0.001},
		[]float64{0.3, 0.15, 0.3, 0.15},
		[]int{6, 6, 6, 6},
		fn,
	)
}
