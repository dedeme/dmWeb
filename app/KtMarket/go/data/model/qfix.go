// Copyright 14-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Model Fix Quantum.
package model

import (
	"github.com/dedeme/KtMarket/data/model/docs/qfixDoc"
	"github.com/dedeme/ktlib/math"
)

func qfixCalc(
	closes [][]float64,
	params []float64,
	refs []float64,
	action func(closes, refs []float64),
) {
	jmp := params[0] + 1.0
	lgJmp := math.Log(jmp)

	downGap := func(q float64) float64 {
		return math.Pow(jmp, math.Round(math.Log(q)/lgJmp, 0)-1.0)
	}
	upGap := func(q float64) float64 {
		return math.Pow(jmp, math.Round(math.Log(q)/lgJmp, 0)+1.0)
	}
	downGap2 := func(q, ref float64) float64 {
		for {
			ref2 := ref * jmp
			if ref2*math.Sqrt(jmp) >= q {
				return ref
			}
			ref = ref2
		}
	}
	upGap2 := func(q, ref float64) float64 {
		for {
			ref2 := ref / jmp
			if ref2/math.Sqrt(jmp) <= q {
				return ref
			}
			ref = ref2
		}
	}

	nCos := len(closes[0])
	pvrow := make([]float64, nCos)
	for i := 0; i < nCos; i++ {
		for _, cs := range closes {
			if cs[i] > 0 {
				pvrow[i] = cs[i]
				break
			}
		}
		if pvrow[i] == 0 {
			pvrow[i] = 1
		}
	}

	for i, c := range pvrow {
		if refs[i] < 0 {
			refs[i] = downGap(c) / jmp
		}
	}

	for _, row := range closes {
		var newRefs []float64

		for i, q := range row {
			if q < 0 {
				newRefs = append(newRefs, refs[i])
				continue
			}
			q0 := pvrow[i]
			ref := refs[i]

			if q0 < ref {
				if q < q0 {
					newRefs = append(newRefs, upGap2(q, ref))
				} else if q > ref {
					newRefs = append(newRefs, downGap(q))
				} else {
					newRefs = append(newRefs, ref)
				}
			} else {
				if q > q0 {
					newRefs = append(newRefs, downGap2(q, ref))
				} else if q < ref {
					newRefs = append(newRefs, upGap(q))
				} else {
					newRefs = append(newRefs, ref)
				}
			}
		}

		pvrow = row
		refs = newRefs
		action(row, refs)
	}
}

func newQfix() *T {
	return &T{
		"QFIJO",
		"Quantum fijo",
		qfixDoc.Get(),
		[]string{
			"Intervalo",
		},
		[]float64{
			0.235,
		},
		[]float64{
			0.140,
		},
		[]int{
			4,
		},
		qfixCalc,
	}
}
