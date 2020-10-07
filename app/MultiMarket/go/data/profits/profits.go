// Copyright 17-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Profits historic entry.
package profits

import (
	"github.com/dedeme/golib/json"
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

func (e *T) ToJs() json.T {
	return json.Wa([]json.T{
		json.Ws(e.date),
		json.Wd(e.total),
		json.Wd(e.acc),
		json.Wd(e.risk),
	})
}

func FromJs(js json.T) *T {
	a := js.Ra()
	return &T{
		a[0].Rs(),
		a[1].Rd(),
		a[2].Rd(),
		a[3].Rd(),
	}
}
