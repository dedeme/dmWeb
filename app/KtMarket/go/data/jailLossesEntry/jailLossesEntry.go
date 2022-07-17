// Copyright 10-Jul-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Jail losses historic entry.
package jailLossesEntry

import (
	"github.com/dedeme/ktlib/js"
)

type T struct {
	// Date in base format (YYYYMMDD)
	Date string
	// Losses (a positive value).
	Losses float64
}

// Create an entry
//   date : Date in base format (YYYYMMDD)
//   losses: Losses (a positive value).
func New(date string, losses float64) *T {
	return &T{date, losses}
}

func (e *T) ToJs() string {
	return js.Wa([]string{
		js.Ws(e.Date),
		js.Wd(e.Losses),
	})
}

func FromJs(j string) *T {
	a := js.Ra(j)
	return &T{
		js.Rs(a[0]),
		js.Rd(a[1]),
	}
}
