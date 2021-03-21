// Copyright 10-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Hueco de volatilidad.
package vgap

import (
	"github.com/dedeme/MultiMarket/data/flea/fmodel"
	"github.com/dedeme/MultiMarket/data/flea/refPos"
	"math"
)

func fn(
	closes [][]float64, params []float64, init *refPos.T,
	action func([]float64, []float64),
) {
	if init != nil {
		panic("init is not nil")
	}
	nDays := len(closes)
	nCos := len(closes[0])
	mul := params[0]
	qmul := 1 / (mul + 1)
	rmul := mul * qmul
	times := params[1]

	toBuys := make([]bool, nCos)
	avgs := make([]float64, nCos)
	dvs := make([]float64, nCos)
	refs := make([]float64, nCos)
	for iCo := 0; iCo < nCos; iCo++ {
		i1Day := -1
		for iDay := 0; iDay < nDays; iDay++ {
			if closes[iDay][iCo] > 0 {
				i1Day = iDay
				break
			}
		}
		if i1Day < 0 {
			panic("i1Day < 0")
		}
		q1 := closes[i1Day][iCo]

		i2Day := -1
		for iDay := i1Day; iDay < nDays; iDay++ {
			q := closes[iDay][iCo]
			if q > 0 && q != q1 {
				i2Day = iDay
				break
			}
		}
		if i2Day < 0 {
			panic("i2Day < 0")
		}
		q2 := closes[i2Day][iCo]

		avg := (q1 + q2) / 2
		avgs[iCo] = avg
		dvs[iCo] = math.Abs(q1-avg) / avg
		refs[iCo] = q2 * (1 - dvs[iCo]*times)
	}

	for iDay := 0; iDay < nDays; iDay++ {
		for iCo := 0; iCo < nCos; iCo++ {
			q := closes[iDay][iCo]
			if q > 0 {
				avg := avgs[iCo]*rmul + q*qmul
				avgs[iCo] = avg
				dvs[iCo] = dvs[iCo]*rmul + math.Abs((q-avg)/avg)*qmul
				ref := refs[iCo]
				if toBuys[iCo] {
					nref := q * (1 + dvs[iCo]*times)
					if nref < ref {
						ref = nref
					}
					if q > ref {
						refs[iCo] = q * (1 - dvs[iCo]*times)
						toBuys[iCo] = false
					} else {
						refs[iCo] = ref
					}
				} else {
					nref := q * (1 - dvs[iCo]*times)
					if nref > ref {
						ref = nref
					}
					if q < ref {
						refs[iCo] = q * (1 + dvs[iCo]*times)
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
		"VGAP",
		"Hueco de volalitidad",
		[]string{"Días", "Mult."},
		[]float64{3, 1},
		[]float64{30, 6},
		[]int{3, 3},
		false,
		fn,
	)
}
