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

// Constructor with parameter.
//    param: Flea parameter.
func New(param float64) *T {
	return &T{param}
}

// Model parameter.
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
//    assets    : Totas assets (amount from 0 to ...)
//    profitsAvg: Ratio of profits average (-1 to ...).
func (f *T) Evaluate(assets, profitsAvg float64) float64 {
	if assets > cts.AssetsMax {
		assets = cts.AssetsMax
	}
	if profitsAvg > cts.ProfitsAvgMax {
		profitsAvg = cts.ProfitsAvgMax
	}
	return (assets*cts.AssetsRatio/cts.AssetsMax +
		(1+profitsAvg)*cts.ProfitsAvgRatio/(1+cts.ProfitsAvgMax)) / 2
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
