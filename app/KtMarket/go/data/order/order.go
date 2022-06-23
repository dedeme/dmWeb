// Copyright 16-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Order for tests
package order

import (
	"github.com/dedeme/ktlib/js"
)

type T struct {
	Date   string
	Nick   string
	IsSell bool
	Stocks int
	Price  float64
}

func New(date, nk string, isSell bool, stocks int, price float64) *T {
	return &T{date, nk, isSell, stocks, price}
}

func ToJs(o *T) string {
	return js.Wa([]string{
		js.Ws(o.Date),
		js.Ws(o.Nick),
		js.Wb(o.IsSell),
		js.Wi(o.Stocks),
		js.Wd(o.Price),
	})
}
