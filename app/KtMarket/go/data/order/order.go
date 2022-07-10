// Copyright 03-Jul-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Market order
package order

import (
	"github.com/dedeme/ktlib/js"
)

const (
	BUY = iota
	SELL
	CATCH
)

// Market order
type T struct {
	// Order date
	Date string
	// Company nick
	Nick string
	// Order type (BUY, SELL, CATCH)
	Type int
	// Stocks number
	Stocks int
	// Price of each stock.
	Price float64
}

func New(date, nk string, tp, stocks int, price float64) *T {
	return &T{date, nk, tp, stocks, price}
}

func ToJs(o *T) string {
	return js.Wa([]string{
		js.Ws(o.Date),
		js.Ws(o.Nick),
		js.Wi(o.Type),
		js.Wi(o.Stocks),
		js.Wd(o.Price),
	})
}

// Returns the number of orders type BUY of 'os'.
func Buys(os []*T) int {
	n := 0
	for _, o := range os {
		if o.Type == BUY {
			n++
		}
	}
	return n
}

// Returns the number of orders type SELL of 'os'.
func Sales(os []*T) int {
	n := 0
	for _, o := range os {
		if o.Type == SELL {
			n++
		}
	}
	return n
}

// Returns the number of orders type CATCH of 'os'.
func Catches(os []*T) int {
	n := 0
	for _, o := range os {
		if o.Type == CATCH {
			n++
		}
	}
	return n
}
