// Copyright 17-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Model last evaluation data
package eval

import (
	"github.com/dedeme/QMarket/data/cts"
	"github.com/dedeme/golib/json"
)

type T struct {
	buys       int
	sales      int
	assets     float64
	profitsAvg float64
	value      float64
}

func New(buys, sales int, assets, profitsAvg float64) *T {
	if assets > cts.AssetsMax {
		assets = cts.AssetsMax
	}
	if profitsAvg > cts.ProfitsAvgMax {
		profitsAvg = cts.ProfitsAvgMax
	}
	v := (assets*cts.AssetsRatio/cts.AssetsMax +
		(1+profitsAvg)*cts.ProfitsAvgRatio/(1+cts.ProfitsAvgMax)) / 2
	return &T{buys, sales, assets, profitsAvg, v}
}

// Number of buy operations
func (e *T) Buys() int {
	return e.buys
}

// Number of sale operations
func (e *T) Sales() int {
	return e.sales
}

// Total assets (euros) for historic simulation.
func (e *T) Assets() float64 {
	return e.assets
}

// Average of companies profits (ratio). Each company profits is:
//  (FinalCapital - InitalCapital) / IntitalCapital
func (e *T) ProfitsAvg() float64 {
	return e.profitsAvg
}

// Evaluation final. It is between 0 and 1
func (e *T) Value() float64 {
	return e.value
}

func (e *T) ToJs() json.T {
	return json.Wa([]json.T{
		json.Wi(e.buys),
		json.Wi(e.sales),
		json.Wd(e.assets),
		json.Wd(e.profitsAvg),
		json.Wd(e.value),
	})
}

func FromJs(js json.T) *T {
	a := js.Ra()
	return &T{
		a[0].Ri(),
		a[1].Ri(),
		a[2].Rd(),
		a[3].Rd(),
		a[4].Rd(),
	}
}
