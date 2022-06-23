// Copyright 19-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Profits historic entry.
package profitsEntry

import (
	"github.com/dedeme/ktlib/js"
)

type T struct {
	date  string
	total float64
	acc   float64
	risk  float64
}

// Create an entry
//   date : Date in base format (YYYYMMDD)
//   total: Total profits.
//   acc  : Accounting profits.
//   risk : Risk profits.
func New(date string, total, acc, risk float64) *T {
	return &T{date, total, acc, risk}
}

// Date in base format (YYYYMMDD)
func (e *T) Date() string {
	return e.date
}

// Total profits.
func (e *T) Total() float64 {
	return e.total
}

// Accouning profits.
func (e *T) Acc() float64 {
	return e.acc
}

// Risk profits.
func (e *T) Risk() float64 {
	return e.risk
}

func (e *T) ToJs() string {
	return js.Wa([]string{
		js.Ws(e.date),
		js.Wd(e.total),
		js.Wd(e.acc),
		js.Wd(e.risk),
	})
}

func FromJs(j string) *T {
	a := js.Ra(j)
	return &T{
		js.Rs(a[0]),
		js.Rd(a[1]),
		js.Rd(a[2]),
		js.Rd(a[3]),
	}
}
