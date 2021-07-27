// Copyright 16-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Daily chart data.
package dailyChart

import (
	"github.com/dedeme/golib/json"
)

// Parameter, stocks and reference of a manager-company.
type DataT struct {
	param  float64
	stocks int
	price  float64
	ref    float64
}

// Constructor
//    param: manager-company parameter if its model is JUMP or JMP2. Oterwise -1.
//    stocks: Stocks in portfolio.
//    price : Price of stocks or 0.
//    ref   : Stop reference.
func DataNew(param float64, stocks int, price, ref float64) *DataT {
	return &DataT{param, stocks, price, ref}
}

func (d *DataT) Stocks() int {
	return d.stocks
}

func (d *DataT) Price() float64 {
	return d.price
}

func (d *DataT) Ref() float64 {
	return d.ref
}

func (d *DataT) toJs() json.T {
	return json.Wa([]json.T{
		json.Wd(d.param),
		json.Wi(d.stocks),
		json.Wd(d.price),
		json.Wd(d.ref),
	})
}

func dailyChartDataFromJs(js json.T) *DataT {
	a := js.Ra()

	return &DataT{
		a[0].Rd(),
		a[1].Ri(),
		a[2].Rd(),
		a[3].Rd(),
	}
}

type T struct {
	nick         string
	close        float64
	hours        []int
	quotes       []float64
	managersData []*DataT
}

// Creates a new daily chart data.
//   nick        : Nick name of a company.
//   close       : Last close.
//   Hours       : Daily quotes hours from before to after.
//   quotes      : Daily quotes from before to after.
//   managersData: Managers accounting result data.
func New(
	nick string, close float64, hours []int, quotes []float64,
	accData []*DataT,
) *T {
	return &T{nick, close, hours, quotes, accData}
}

func (d *T) Nick() string {
	return d.nick
}

func (d *T) Close() float64 {
	return d.close
}

func (d *T) Hours() []int {
	return d.hours
}

func (d *T) Quotes() []float64 {
	return d.quotes
}

func (d *T) ManagersData() []*DataT {
	return d.managersData
}

func (d *T) ToJs() json.T {
	var hours []json.T
	for _, e := range d.hours {
		hours = append(hours, json.Wi(e))
	}
	var quotes []json.T
	for _, e := range d.quotes {
		quotes = append(quotes, json.Wd(e))
	}
	var managersData []json.T
	for _, e := range d.managersData {
		managersData = append(managersData, e.toJs())
	}

	return json.Wa([]json.T{
		json.Ws(d.nick),
		json.Wd(d.close),
		json.Wa(hours),
		json.Wa(quotes),
		json.Wa(managersData),
	})
}

func FromJs(js json.T) *T {
	a := js.Ra()
	var hours []int
	for _, e := range a[2].Ra() {
		hours = append(hours, e.Ri())
	}
	var quotes []float64
	for _, e := range a[3].Ra() {
		quotes = append(quotes, e.Rd())
	}
	var managersData []*DataT
	for _, e := range a[4].Ra() {
		managersData = append(managersData, dailyChartDataFromJs(e))
	}
	return &T{
		a[0].Rs(),
		a[1].Rd(),
		hours,
		quotes,
		managersData,
	}
}
