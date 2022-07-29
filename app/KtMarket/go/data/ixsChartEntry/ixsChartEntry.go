// Copyright 21-Jul-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Indexes chart entry.
package ixsChartEntry

import (
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/time"
)

type T struct {
	// Reading hour
	Hour int
	// Investors assets
	Invs []float64
	// Indexes quotes
	Ixs []float64
}

func New(hour int, invs, ixs []float64) *T {
	return &T{hour, invs, ixs}
}

func NewNow(invs, ixs []float64) *T {
	return &T{time.Hour(time.Now()), invs, ixs}
}

func ToJs(e *T) string {
	return js.Wa([]string{
		js.Wi(e.Hour),
		js.Wa(arr.Map(e.Invs, func(n float64) string {
			return js.WdDec(n, 2)
		})),
		js.Wa(arr.Map(e.Ixs, func(n float64) string {
			return js.WdDec(n, 2)
		})),
	})
}

func FromJs(j string) *T {
	a := js.Ra(j)
	return New(
		js.Ri(a[0]),
		arr.Map(js.Ra(a[1]), js.Rd),
		arr.Map(js.Ra(a[2]), js.Rd),
	)
}
