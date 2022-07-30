// Copyright 14-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Model Fix Quantum.
package model

import (
	"github.com/dedeme/KtMarket/data/model/docs/qfixDoc"
	"github.com/dedeme/KtMarket/data/reference"
	"github.com/dedeme/ktlib/math"
)

func qfixCalc(
	closes [][]float64,
	params []float64,
	refs []*reference.T,
	action func(closes []float64, refs []*reference.T),
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
	for i, cl := range closes[0] {
		pvrow[i] = cl
		if refs[i].Ref < 0 {
			refs[i] = reference.New(downGap(cl)/jmp, true)
		}
	}

	for _, row := range closes {
		var newRefs []*reference.T

		for i, q := range row {
			q0 := pvrow[i]
			ref := refs[i]

			if q0 >= ref.Ref { // InPortfolio
				if q > q0 {
					newRefs = append(newRefs, reference.New(downGap2(q, ref.Ref), true))
				} else if q < ref.Ref {
					newRefs = append(newRefs, reference.New(upGap(q), false))
				} else {
					newRefs = append(newRefs, ref)
				}
			} else {
				if q < q0 {
					newRefs = append(newRefs, reference.New(upGap2(q, ref.Ref), false))
				} else if q > ref.Ref {
					newRefs = append(newRefs, reference.New(downGap(q), true))
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
