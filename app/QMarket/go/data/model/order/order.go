// Copyright 05-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Market order.
package order

import (
	"github.com/dedeme/golib/json"
)

type T struct {
	date   string
	nick   string
	isSale bool
	stocks int
	price  float64
}

// Constructor
func New(
	date, nick string, isSale bool, stocks int, price float64,
) *T {
	return &T{date, nick, isSale, stocks, price}
}

func (o *T) Date() string {
	return o.date
}

func (o *T) Nick() string {
	return o.nick
}

func (o *T) IsSale() bool {
	return o.isSale
}

func (o *T) Stocks() int {
	return o.stocks
}

func (o *T) Price() float64 {
	return o.price
}

func (o *T) ToJs() json.T {
	return json.Wa([]json.T{
		json.Ws(o.date),
		json.Ws(o.nick),
		json.Wb(o.isSale),
		json.Wi(o.stocks),
		json.Wd(o.price),
	})
}
