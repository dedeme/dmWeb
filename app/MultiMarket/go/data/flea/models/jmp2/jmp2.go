// Copyright 05-Dec-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Differencial investor model (2).
package jmp2

import (
	"github.com/dedeme/MultiMarket/data/flea/fmodel"
	"math"
)

func downGap(q, ref, jmp float64) float64 {
	for {
		ref2 := ref * jmp
		if ref2*math.Sqrt(jmp) >= q {
			return ref
		}
		ref = ref2
	}
}

func upGap(q, ref, jmp float64) float64 {
	for {
		ref2 := ref / jmp
		if ref2/math.Sqrt(jmp) <= q {
			return ref
		}
		ref = ref2
	}
}

func fn(
	closes [][]float64, params []float64, action func([]float64, []float64),
) {
	nDays := len(closes)
	nCos := len(closes[0])
	jmp := params[0] + 1.0

	toBuys := make([]bool, nCos)
	refs := make([]float64, nCos)
	preqs := make([]float64, nCos)
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
		refs[iCo] = q / jmp / jmp
		preqs[iCo] = q
	}

	for iDay := 0; iDay < nDays; iDay++ {
		for iCo := 0; iCo < nCos; iCo++ {
			q := closes[iDay][iCo]
			if q > 0 {
				ref := refs[iCo]
				preq := preqs[iCo]
				preqs[iCo] = q
				if toBuys[iCo] {
					if q < preq {
						newRef := upGap(q, ref, jmp)
						if newRef < ref {
							refs[iCo] = newRef
						}
					} else if q > ref {
						refs[iCo] = q / jmp / jmp
						toBuys[iCo] = false
					}
				} else {
					if q > preq {
						newRef := downGap(q, ref, jmp)
						if newRef > ref {
							refs[iCo] = newRef
						}
					} else if q < ref {
						refs[iCo] = q * jmp * jmp
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
		"JMP2",
		"Jump2",
		[]string{"Salto"},
		[]float64{0.01},
		[]float64{0.25},
		[]int{6},
		fn,
	)
}
