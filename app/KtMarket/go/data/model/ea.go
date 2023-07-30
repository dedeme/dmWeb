// Copyright 25-Jun-2023 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Exponential mobil average.

package model

import (
	"github.com/dedeme/KtMarket/data/model/docs/eaDoc"
	"github.com/dedeme/KtMarket/data/reference"
)

func eaCalc(
	closes [][]float64,
	params []float64,
	refs []*reference.T,
	action func(closes []float64, refs []*reference.T),
) {
	days := int(params[0] + 0.4)
	strip := params[1]
	nCos := len(closes[0])

	avgs := make([]float64, nCos)
	isNews := make([]bool, nCos)
	for i, r := range refs {
		if r.Ref < 0 {
			isNews[i] = true
		}
	}

	for day, cs := range closes {
		newAvgs := make([]float64, nCos)
		newRefs := make([]*reference.T, nCos)

		for i, c := range cs {
			isNew := isNews[i]
			if day < days && isNew {
				newAvgs[i] = avgs[i] + c
				newRefs[i] = reference.New(c*(1-strip), true)
			} else if day == days && isNew {
				avg := avgs[i] / float64(days)
				newAvg := avg + 2*(c-avg)/float64((days+1))

				newAvgs[i] = newAvg
				if newAvg > c {
					newRefs[i] = reference.New(newAvg*(1+strip), false)
				} else {
					newRefs[i] = reference.New(newAvg*(1-strip), true)
				}
			} else {
				avg := avgs[i]
				rf := refs[i]
				newAvg := avg + 2*(c-avg)/float64((days+1))
				newAvgs[i] = newAvg

				if rf.InPortfolio {
					if c < rf.Ref {
						newRefs[i] = reference.New(newAvg*(1+strip), false)
					} else {
						newRf := newAvg * (1 - strip)
						if newRf > rf.Ref {
							newRefs[i] = reference.New(newRf, true)
						} else {
							newRefs[i] = reference.New(rf.Ref, true)
						}
					}
				} else {
					if c > rf.Ref {
						newRefs[i] = reference.New(newAvg*(1-strip), true)
					} else {
						newRf := newAvg * (1 + strip)
						if newRf < rf.Ref {
							newRefs[i] = reference.New(newRf, false)
						} else {
							newRefs[i] = reference.New(rf.Ref, false)
						}
					}
				}
			}
		}

		avgs = newAvgs
		refs = newRefs

		action(cs, refs)
	}
}

func newEa() *T {
	return &T{
		"ME",
		"Media móvil exponencial",
		eaDoc.Get(),
		[]string{
			"Días",
			"Banda",
		},
		[]float64{
			44.0,
			0.057,
		},
		[]float64{
			6.0,
			0.003,
		},
		[]int{
			0,
			4,
		},
		eaCalc,
	}
}
