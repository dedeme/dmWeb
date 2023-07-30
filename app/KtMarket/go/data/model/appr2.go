// Copyright 14-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Model Approximations.
package model

import (
	"github.com/dedeme/KtMarket/data/model/docs/appr2Doc"
	"github.com/dedeme/KtMarket/data/reference"
)

func appr2Calc(
	closes [][]float64,
	params []float64,
	refs []*reference.T,
	action func(closes []float64, refs []*reference.T),
) {
	start := params[0]
	incr := params[1]
	nCos := len(closes[0])

	for i, c := range closes[0] {
		if refs[i].Ref < 0 {
			refs[i] = reference.New(c*(1-start), true)
		}
	}

	for _, cs := range closes {
		newRefs := make([]*reference.T, nCos)

		for i, c := range cs {
			rf := refs[i]

			if rf.InPortfolio {
				if c < rf.Ref {
					newRefs[i] = reference.New(c*(1+start), false)
				} else {
          r1 := rf.Ref+(c-rf.Ref)*incr
          r2 := c * (1-start)
          if r1 > r2 {
            newRefs[i] = reference.New(r1, true)
          } else {
            newRefs[i] = reference.New(r2, true)
          }
				}
			} else {
				if c > rf.Ref {
					newRefs[i] = reference.New(c*(1-start), true)
				} else {
					r1 := rf.Ref-(rf.Ref-c)*incr
          r2 := c * (1+start)
          if r1 < r2 {
            newRefs[i] = reference.New(r1, false)
          } else {
            newRefs[i] = reference.New(r2, false)
          }
				}
			}

		}
		refs = newRefs

		action(cs, refs)
	}

}

func newAppr2() *T {
	return &T{
		"APRX2",
		"Aproximaciones sucesivas (2)",
		appr2Doc.Get(),
		[]string{
			"Inicio",
			"Aproximación",
		},
		[]float64{
			0.20,
			0.114,
		},
		[]float64{
			0.01,
			0.006,
		},
		[]int{
			4,
			4,
		},
		appr2Calc,
	}
}
