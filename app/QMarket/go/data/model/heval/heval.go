// Copyright 17-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Model historic evaluation data
package heval

import (
	"github.com/dedeme/golib/json"
)

type T struct {
	sales float64
	value float64
}

func New(sales float64, value float64) *T {
	return &T{sales, value}
}

func (he *T) Sales() float64 {
	return he.sales
}

func (he *T) Value() float64 {
	return he.value
}

func (he *T) Update(days int, lastSales int, lastValue float64) *T {
	sales := (he.sales*float64(days) + float64(lastSales)) / float64(days+1)
	value := (he.value*float64(days) + float64(lastValue)) / float64(days+1)
	return &T{sales, value}
}

func (he *T) ToJs() json.T {
	return json.Wa([]json.T{
		json.Wd(he.sales),
		json.Wd(he.value),
	})
}

func FromJs(js json.T) *T {
	a := js.Ra()
	return &T{
		a[0].Rd(),
		a[1].Rd(),
	}
}
