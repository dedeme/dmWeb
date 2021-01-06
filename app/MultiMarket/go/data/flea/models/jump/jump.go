// Copyright 05-Dec-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Differencial investor model.
package jump

import (
	"github.com/dedeme/MultiMarket/data/flea/fmodel"
	"github.com/dedeme/MultiMarket/data/flea/refPos"
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

/*
func GapTests() {
	assert := func(msg string, val bool) {
		if !val {
			panic(msg)
		}
	}
	assert("Up(0.5, 0.1) == 0.564",
		fmt.Sprintf("%.3f", upGap(0.5, 1.1, math.Log(1.1))) == "0.564")
	assert("Up(1, 0.1) == 1.1",
		fmt.Sprintf("%.3f", upGap(1, 1.1, math.Log(1.1))) == "1.100")
	assert("Up(50, 0.1) == 1.1",
		fmt.Sprintf("%.3f", upGap(50, 1.1, math.Log(1.1))) == "54.764")
	assert("Up(100, 0.1) == 1.1",
		fmt.Sprintf("%.3f", upGap(100, 1.1, math.Log(1.1))) == "106.719")

	assert("Down(0.5, 0.1) == 0.564",
		fmt.Sprintf("%.3f", downGap(0.5, 1.1, math.Log(1.1))) == "0.467")
	assert("Down(1, 0.1) == 1.1",
		fmt.Sprintf("%.3f", downGap(1, 1.1, math.Log(1.1))) == "0.909")
	assert("Down(50, 0.1) == 1.1",
		fmt.Sprintf("%.3f", downGap(50, 1.1, math.Log(1.1))) == "45.259")
	assert("Down(100, 0.1) == 1.1",
		fmt.Sprintf("%.3f", downGap(100, 1.1, math.Log(1.1))) == "88.197")

	fmt.Println ("Tests ok");
}
*/

func fn(
	closes [][]float64, params []float64, init *refPos.T,
	action func([]float64, []float64),
) {
	nDays := len(closes)
	nCos := len(closes[0])
	jmp := params[0] + 1.0
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
		refs[iCo] = downGap(q, jmp, lgJump)
		preqs[iCo] = q
	}

	if init != nil {
		if len(closes[0]) > 1 {
			panic("Closes is > 1")
		}
		refs[0] = init.Ref()
		toBuys[0] = init.ToBuy()
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
						refs[iCo] = downGap(q, jmp, lgJump) / jmp
						toBuys[iCo] = false
					}
				} else {
					if q > preq {
						newRef := downGap2(q, ref, jmp)
						if newRef > ref {
							refs[iCo] = newRef
						}
					} else if q < ref {
						refs[iCo] = upGap(q, jmp, lgJump) * jmp
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
		"JUMP",
		"Jump",
		[]string{"Salto"},
		[]float64{0.01},
		[]float64{0.25},
		[]int{6},
		true,
		fn,
	)
}
