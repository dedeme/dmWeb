// Copyright 20-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Performance data.
package performance

import (
	"github.com/dedeme/golib/json"
)

type T struct {
	nick          string
	date          string
	isSell        bool
	stocks        int
	expectedPrice float64
	actualPrice   float64
}

// Constructor
func New(
	nick, date string, isSell bool,
	stocks int, expectedPrice, actualPrice float64,
) *T {
	return &T{nick, date, isSell, stocks, expectedPrice, actualPrice}
}

func (p *T) Nick() string {
	return p.nick
}

func (p *T) Date() string {
	return p.date
}

func (p *T) IsSell() bool {
	return p.isSell
}

func (p *T) Stocks() int {
	return p.stocks
}

func (p *T) ExpectedPrice() float64 {
	return p.expectedPrice
}

func (p *T) ActualPrice() float64 {
	return p.actualPrice
}

func (p *T) ToJs() json.T {
	return json.Wa([]json.T{
		json.Ws(p.nick),
		json.Ws(p.date),
		json.Wb(p.isSell),
		json.Wi(p.stocks),
		json.Wd(p.expectedPrice),
		json.Wd(p.actualPrice),
	})
}

func FromJs(js json.T) *T {
	a := js.Ra()
	return New(
		a[0].Rs(),
		a[1].Rs(),
		a[2].Rb(),
		a[3].Ri(),
		a[4].Rd(),
		a[5].Rd(),
	)
}
