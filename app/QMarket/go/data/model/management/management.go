// Copyright 05-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Model market operations management.
package management

import (
	"math"
)

func downGap(q, d, jmp, lgJump float64) float64 {
	return math.Pow(jmp, math.Round(math.Log(q/d)/lgJump)-1.0) * d
}

func upGap(q, d, jmp, lgJump float64) float64 {
	return math.Pow(jmp, math.Round(math.Log(q/d)/lgJump)+1.0) * d
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

func upGap2(q, ref, jmp float64) float64 {
	for {
		ref2 := ref / jmp
		if ref2/math.Sqrt(jmp) <= q {
			return ref
		}
		ref = ref2
	}
}

// Makes buys and sales using 'closes' and executes 'action' every day.
//    closes: Matrix [days][company closes]
//    qlevel: Model quantum level.
//    param : Model parameter.
//    action: Function to do every day with next parameters:
//              dailyCloses: Daily closes for each company (a row of 'closes')
//              dailyRefs  : Reference closes corresponding to 'dailyCloses'
func Run(
	closes [][]float64, qlevel int, param float64,
	action func([]float64, []float64),
) {
	nDays := len(closes)
	nCos := len(closes[0])
	d := 1 + (param * float64(qlevel) / 3)
	jmp := param + 1.0
	lgJump := math.Log(jmp)

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
		refs[iCo] = downGap(q, d, jmp, lgJump)
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
						newRef := upGap2(q, ref, jmp)
						if newRef < ref {
							refs[iCo] = newRef
						}
					} else if q > ref {
						refs[iCo] = downGap(q, d, jmp, lgJump)
						toBuys[iCo] = false
					}
				} else {
					if q > preq {
						newRef := downGap2(q, ref, jmp)
						if newRef > ref {
							refs[iCo] = newRef
						}
					} else if q < ref {
						refs[iCo] = upGap(q, d, jmp, lgJump)
						toBuys[iCo] = true
					}
				}
			}
		}
		action(closes[iDay], refs)
	}
}
