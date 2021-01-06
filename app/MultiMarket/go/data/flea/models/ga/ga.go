// Copyright 10-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Geometric average investor model.
package ga

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
	mul := params[0]
	qmul := 1 / (mul + 1)
	rmul := mul * qmul
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
		refs2[iCo] = closes[ixDay][iCo] * (1.0 - stripToSell)
	}

	if init != nil {
		if len(closes[0]) > 1 {
			panic("Closes is > 1")
		}
		refs2[0] = init.Ref()
		toBuys[0] = init.ToBuy()
	}

	for iDay := 0; iDay < nDays; iDay++ {
		for iCo := 0; iCo < nCos; iCo++ {
			q := closes[iDay][iCo]
			if q > 0 {
				refICo := refs[iCo]
				ref := refICo*rmul + q*qmul
				if toBuys[iCo] {
					if ref > refICo {
						ref = refICo
					}
					refs[iCo] = ref
					ref2 := ref * (1 + stripToBuy)
					refs2[iCo] = ref2
					if q > ref2 {
						toBuys[iCo] = false
					}
				} else {
					if ref < refICo {
						ref = refICo
					}
					refs[iCo] = ref
					ref2 := ref * (1 - stripToSell)
					refs2[iCo] = ref2
					if q < ref2 {
						toBuys[iCo] = true
					}
				}
			}
		}
		action(closes[iDay], refs2)
	}
}

func Mk() *fmodel.T {
	return fmodel.New(
		"GA",
		"Media Geométrica",
		[]string{"Días", "Banda C", "Banda V"},
		[]float64{30, 0.001, 0.001},
		[]float64{120, 0.1, 0.1},
		[]int{3, 6, 6},
		true,
		fn,
	)
}
