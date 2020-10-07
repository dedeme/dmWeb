// Copyright 11-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Evaluated flea data with date.
package evalDate

import (
	"github.com/dedeme/MultiMarket/data/flea/eval"
	"github.com/dedeme/golib/json"
)

type T struct {
	date  string
	eflea *eval.T
}

// Constructor
//    date : Date of evaluation.
//    eflea: Evaluated flea.
func New(date string, eflea *eval.T) *T {
	return &T{date, eflea}
}

// Date of evaluation.
func (e *T) Date() string {
	return e.date
}

// Evaluated flea.
func (e *T) Eflea() *eval.T {
	return e.eflea
}

func (e *T) ToJs() json.T {
	return json.Wa([]json.T{
		json.Ws(e.date),
		e.eflea.ToJs(),
	})
}

func FromJs(js json.T) *T {
	a := js.Ra()
	return &T{
		a[0].Rs(),
		eval.FromJs(a[1]),
	}
}
