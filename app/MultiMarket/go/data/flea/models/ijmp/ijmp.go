// Copyright 08-Dec-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Increment jump investor model.
package ijmp

import (
	"github.com/dedeme/MultiMarket/data/flea/fmodel"
	"math"
)

func upGap(q, jmp, lgJump float64) float64 {
	return math.Pow(jmp, math.Round(math.Log(q)/lgJump)+1.0)
}

func downGap2(q, ref, jmp float64) float64 {
	for {
		ref2 := ref * jmp
		if ref2*math.Sqrt(jmp) >= q {
			return ref
		}
		ref = ref2
	}
}

func downGap(q, jmp, lgJump float64) float64 {
	return math.Pow(jmp, (math.Round(math.Log(q)/lgJump))-1.0)
}

func upGap2(q, ref, jmp float64) float64 {
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
	days := int(params[0])
	jmp := params[1] + 1.0
	lgJump := math.Log(jmp)

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
		refs[iCo] = downGap(closes[ixDay][iCo], jmp, lgJump)
	}

	for iDay := 0; iDay < nDays; iDay++ {
		for iCo := 0; iCo < nCos; iCo++ {
			if iDay < days {
				refs2[iCo] = -1
				continue
			}

			q := closes[iDay][iCo]
			if q > 0 {
				ref := refs[iCo]
				if toBuys[iCo] {
					oldq := closes[iDay-days][iCo]
					if oldq > 0 && oldq < ref {
						ref = upGap2(oldq, ref, jmp)
					}
					if q > ref {
						oldq := closes[iDay-days][iCo]
						if oldq < 0 {
							oldq = ref
						}
						ref = downGap(oldq, jmp, lgJump)
						toBuys[iCo] = false
					}
				} else {
					oldq := closes[iDay-days][iCo]
					if oldq > 0 && oldq > ref {
						ref = downGap2(oldq, ref, jmp)
					}
					if q < ref {
						oldq := closes[iDay-days][iCo]
						if oldq < 0 {
							oldq = ref
						}
						ref = upGap(oldq, jmp, lgJump)
						toBuys[iCo] = true
					}
				}
				refs[iCo] = ref
			}
			refs2[iCo] = refs[iCo]
		}
		action(closes[iDay], refs2)
	}
}

func Mk() *fmodel.T {
	return fmodel.New(
		"IJMP",
		"Incremental Jump",
		[]string{"Días", "Salto"},
		[]float64{20, 0.01},
		[]float64{120, 0.25},
		[]int{0, 6},
		fn,
	)
}
