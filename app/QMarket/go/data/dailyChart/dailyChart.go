// Copyright 12-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Daily chart data.
package dailyChart

import (
	"github.com/dedeme/QMarket/data/acc"
	"github.com/dedeme/QMarket/data/cts"
	"github.com/dedeme/QMarket/data/investors"
	"github.com/dedeme/QMarket/data/model"
	"github.com/dedeme/QMarket/data/nick"
	"github.com/dedeme/QMarket/data/nickFloat"
	"github.com/dedeme/QMarket/data/qtable"
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
//    param : investor-company parameter.
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
	nick          string
	close         float64
	hours         []int
	quotes        []float64
	investorsData []*DataT
}

// Creates a new daily chart data.
//   nick         : Nick name of a company.
//   close        : Last close.
//   Hours        : Daily quotes hours from before to after.
//   quotes       : Daily quotes from before to after.
//   investorsData: Investors accounting result data.
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

func (d *T) InvestorsData() []*DataT {
	return d.investorsData
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
	var investorsData []json.T
	for _, e := range d.investorsData {
		investorsData = append(investorsData, e.toJs())
	}

	return json.Wa([]json.T{
		json.Ws(d.nick),
		json.Wd(d.close),
		json.Wa(hours),
		json.Wa(quotes),
		json.Wa(investorsData),
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
	var investorsData []*DataT
	for _, e := range a[4].Ra() {
		investorsData = append(investorsData, dailyChartDataFromJs(e))
	}
	return &T{
		a[0].Rs(),
		a[1].Rd(),
		hours,
		quotes,
		investorsData,
	}
}

// Makes the daily initial chart.
//    Closes: Companies closes.
//    pfs   : [Qlevel][entries]PfEntryT. Portfolio entries for each
//            investor (Qlevel)
//    invsTb: Investors table
//    hour  : Hour of getting 'qs'
//    qs    : Last read quotes.
//    nkCl  : Pair nick-lastClose
func Initial(
	closes *qtable.T, pfs [][]*acc.PfEntryT, invsTb *investors.TableT,
	hour int, qs []*nick.QvalueT, nkCl *nickFloat.T,

) *T {
	nk := nkCl.Nick()
	nickName := nk.Name()
	q := nkCl.Value()

	for _, e := range qs {
		if e.Nick() == nk.Id() {
			q = e.Value()
		}
	}

	hours := []int{hour, hour}
	quotes := []float64{nkCl.Value(), q}

	var investorsData []*DataT

	for inv := 0; inv < cts.Qlevels; inv++ {
		stocks := 0
		price := 0.0
		for _, pfe := range pfs[inv] {
			if pfe.Nick() == nickName {
				stocks = pfe.Stocks()
				price = pfe.Price()
			}
		}

		paramId := invsTb.Base
		if pars, ok := invsTb.Params[nickName]; ok {
			paramId = pars[inv]
		}
		md := model.New(inv, paramId)

		ref := nkCl.Value()
		cls, ok := closes.NickValues(nickName)
		if ok {
			refs := md.Refs(cls)
			ref = refs[len(refs)-1]
		}

		investorsData = append(
			investorsData, DataNew(md.Param(), stocks, price, ref),
		)
	}

	return New(nickName, nkCl.Value(), hours, quotes, investorsData)
}
