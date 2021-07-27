// Copyright 11-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Flea data.
package flea

import (
	"github.com/dedeme/MultiMarket/data/cts"
	"github.com/dedeme/MultiMarket/global/fn"
	"github.com/dedeme/golib/json"
)

type T struct {
	param float64
}

// Constructor with parameters
// Random constructor
//    md: Flea model.
//    date: Date of creation.
//    cycle: Cycle of creation.
//    id: Identifier inside date-cycle.
//    params: Flea parameters.
func New(param float64) *T {
	return &T{param}
}

// Model parameters.
func (f *T) Param() float64 {
	return f.param
}

// Two fleas are equals if they have equals 'params'.
func (f *T) Eq(f2 *T) (ok bool) {
	return fn.Eq(f.param, f2.param, 0.0000001)
}

// Evaluate a flea.
//
// Ratios of assets and profitsAvg are 0 -> -100%, 1 -> 0%, 2 -> 100%,
// 3 -> 200%...
//    assets    : Ratio of assets
//    profitsAvg: Ratio of profits average.
//    profitsSd : Ratio of profits standard deviation (between 0 and 1).
func (f *T) Evaluate(assets, profitsAvg, profitsSd float64) float64 {
	normalize := func(n, min, maxSubMin float64) float64 {
		n = (n - min) / maxSubMin
		if n < 0 {
			return 0.0
		}
		if n > 1 {
			return 1.0
		}
		return n
	}

	pond := 1.0

	if assets < cts.AssetPenalize {
		pond = 0.5
	}

	if profitsAvg < cts.AvgPenalize {
		pond = pond * 0.5
	}

	if profitsSd > cts.StdPenalize {
		pond = pond * 0.5
	}

	assets = normalize(assets, 0.15, 2.35)
	profitsAvg = normalize(profitsAvg, 0.33, 1.47)

	e := assets*float64(cts.AssetsRatio) +
		profitsAvg*(float64(cts.ProfitsAvgRatio)+
			(1-profitsSd)*float64(cts.ProfitsSdRatio))
	return e * pond
}

func (f *T) ToJs() json.T {
	return json.Wa([]json.T{
		json.Wd(f.param),
	})
}

func FromJs(js json.T) *T {
	a := js.Ra()
	return &T{
		a[0].Rd(),
	}
}
