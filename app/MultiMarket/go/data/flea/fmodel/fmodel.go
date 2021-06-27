// Copyright 08-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Flea model
package fmodel

import (
	broker "github.com/dedeme/MultiMarket/data/brokerF"
	"github.com/dedeme/MultiMarket/data/cts"
	"github.com/dedeme/MultiMarket/data/flea/refPos"
	"github.com/dedeme/MultiMarket/data/qtable"
	"github.com/dedeme/golib/json"
	"math"
	"sort"
)

// -- AssetsRsT ----------------------------------------------------------------

type AssetsRsT struct {
	assets float64
	buys   int
	sells  int
}

// Constructor
//    assets: Money.
//    buys  : Buys number.
//    sells : Sell number.
func NewAssetsRs(assets float64, buys, sells int) *AssetsRsT {
	return &AssetsRsT{assets, buys, sells}
}

// Money
func (rs *AssetsRsT) Assets() float64 {
	return rs.assets
}

// Buys number
func (rs *AssetsRsT) Buys() int {
	return rs.buys
}

// Sells number
func (rs *AssetsRsT) Sells() int {
	return rs.sells
}

func (rs *AssetsRsT) ToJs() json.T {
	return json.Wa([]json.T{
		json.Wd(rs.assets),
		json.Wi(rs.buys),
		json.Wi(rs.sells),
	})
}

// -- OrderT -------------------------------------------------------------------

type OrderT struct {
	date   string
	nick   string
	isSell bool
	stocks int
	price  float64
}

// Constructor
func NewOrder(
	date, nick string, isSell bool, stocks int, price float64,
) *OrderT {
	return &OrderT{date, nick, isSell, stocks, price}
}

func (o *OrderT) Date() string {
	return o.date
}

func (o *OrderT) Nick() string {
	return o.nick
}

func (o *OrderT) IsSell() bool {
	return o.isSell
}

func (o *OrderT) Stocks() int {
	return o.stocks
}

func (o *OrderT) Price() float64 {
	return o.price
}

func (o *OrderT) ToJs() json.T {
	return json.Wa([]json.T{
		json.Ws(o.date),
		json.Ws(o.nick),
		json.Wb(o.isSell),
		json.Wi(o.stocks),
		json.Wd(o.price),
	})
}

// -- dailyOrderT --------------------------------------------------------------

type dailyOrderT struct {
	isSell bool
	iCo    int
	pond   float64
}

func newDailyOrder(isSell bool, iCo int, pond float64) *dailyOrderT {
	return &dailyOrderT{isSell, iCo, pond}
}

// -- T ------------------------------------------------------------------------

type T struct {
	id       string
	name     string
	parNames []string
	parMins  []float64
	parMaxs  []float64
	parDecs  []int
	withInit bool
	fcalc    func([][]float64, []float64, *refPos.T, func([]float64, []float64))
}

// Constructor
//    id: Brief identifier. 4 characters maximum (e.g. APPR).
//    name: Model name.
//    parNames: Parameter names.
//    parMins: Minimum values for parameters.
//    parMaxs: Maximum values for parameters.
//    parDecs : Decimal number of parameters.
//              Expected values:
//              4, 6 : Percentage format
//              other: No meaning.
//    withInit: "true" if fcalc admit a value not nil for 'init'
//    fcalc   : Function with the following paramenters:
//              closes ([][]float64): Closes of serveral companies from before
//                                    to after.
//                                    Its rows are 'days' and its columns
//                                    'companies'.
//              params ([]float64): Parameters to do calculations.
//              init (*refPos.T): Intialization or nil. Only is not nil
//                                when closes is [][1]float64.
//              action (func): Function to be called after calculations. Its
//                             parameters are:
//                             -closes ([]float64): Day closes of every company.
//                             -stops ([]float64): Day refs of every company.
//                             This function is defined in 'Refs', 'Profits',
//                               'Orders' and 'Assets'
func New(
	id, name string,
	parNames []string, parMins, parMaxs []float64, parDecs []int,
	withInit bool,
	fcalc func([][]float64, []float64, *refPos.T, func([]float64, []float64)),
) *T {
	return &T{id, name, parNames, parMins, parMaxs, parDecs, withInit, fcalc}
}

// Brief identifier. 4 characters maximum (e.g. APPR).
func (md *T) Id() string {
	return md.id
}

// Model name.
func (md *T) Name() string {
	return md.name
}

// Parameter names.
func (md *T) ParNames() []string {
	return md.parNames
}

// Minimum values for parameters.
func (md *T) ParMins() []float64 {
	return md.parMins
}

// Maximum values for parameters.
func (md *T) ParMaxs() []float64 {
	return md.parMaxs
}

// Decimal number of parameters.
//    Expected values:
//      4, 6 : Percentage format
//      other: No meaning.
func (md *T) ParDecs() []int {
	return md.parDecs
}

// "true" if function returned by Fcalc() admits a value not nil for 'init'
func (md *T) WithInit() bool {
	return md.withInit
}

// Function with the following paramenters:
//    closes ([][]float64): Closes of serveral companies from before to after.
//                          Its rows are 'days' and its columns 'companies'.
//    params ([]float64): Parameters to do calculations.
//    init (*refPos.T): Intialization or nil. Only is not nil
//                      when closes is [][1]float64.
//    action (func): Function to be called after calculations. Its
//                   parameters are:
//                     -closes ([]float64): Day closes of every company.
//                     -stops ([]float64): Day stops of every company.
//                   This function is defined in 'Refs', 'Profits', 'Orders'
//                   and 'Assets'
func (md *T) Fcalc() func(
	[][]float64, []float64, *refPos.T, func([]float64, []float64)) {
	return md.fcalc
}

func (md *T) ToJs() json.T {
	var pnames []json.T
	for _, e := range md.parNames {
		pnames = append(pnames, json.Ws(e))
	}
	var pmins []json.T
	for _, e := range md.parMins {
		pmins = append(pmins, json.Wd(e))
	}
	var pmaxs []json.T
	for _, e := range md.parMaxs {
		pmaxs = append(pmaxs, json.Wd(e))
	}
	var pdecs []json.T
	for _, e := range md.parDecs {
		pdecs = append(pdecs, json.Wi(e))
	}
	return json.Wa([]json.T{
		json.Ws(md.id),
		json.Ws(md.name),
		json.Wa(pnames),
		json.Wa(pmins),
		json.Wa(pmaxs),
		json.Wa(pdecs),
	})
}

func withdrawal(
	cs []float64, nCos int, stockss []int, safe, cash float64,
) (
	newSafe, newCash float64,
) {
	tmpAssets := cash
	for i := 0; i < nCos; i++ {
		stocks := stockss[i]
		if stocks > 0 {
			q := cs[i]
			if q >= 0 {
				tmpAssets += broker.Sell(stocks, q)
			}
		}
	}
	if tmpAssets > cts.InitialCapital+cts.Bet+cts.Bet {
		dif := tmpAssets - cts.InitialCapital - cts.Bet
		if cash >= dif+1000 {
			safe += dif
			cash -= dif
		} else if cash >= cts.Bet+1000 {
      dif = float64(int((cash - 1000)/cts.Bet))*cts.Bet
			safe += dif
			cash -= dif
		}
	}
	return safe, cash
}

// Calculates assets of a investor.
//    opens : Open quotes table.
//    closes: Close quotes table.
//    params: Parameters to calculate.
func (md *T) Assets(opens, closes *qtable.T, params []float64) *AssetsRsT {
	nCos := len(opens.Nicks())
	ops := opens.Values()
	iops := 0
	cls := closes.Values()

	safe := float64(0)
	cash := float64(cts.InitialCapital)
	buys := 0
	sells := 0

	stockss := make([]int, nCos)
	toBuys := make([]bool, nCos)

	// Auxiliar function ---------------------------

	var dorders []*dailyOrderT
	fn := func(cs []float64, refs []float64) {
		os := ops[iops]
		iops++

		// Execute orders //

		sort.Slice(dorders, func(i, j int) bool {
			o1 := dorders[i]
			o2 := dorders[j]
			if o1.isSell {
				return true
			}
			if o2.isSell {
				return false
			}
			return o1.pond > o2.pond
		})

		var newDorders []*dailyOrderT
		sold := false
		for _, o := range dorders {
			iCo := o.iCo
			if o.isSell {
				stocks := stockss[iCo]
				if stocks > 0 {
					q := os[iCo]
					if q >= 0 {
						cash += broker.Sell(stocks, q)
						sells++
						stockss[iCo] = 0
						sold = true
					} else {
						newDorders = append(newDorders, o)
					}
				}
			} else {
				if cash > cts.MinToBet {
					q := os[iCo]
					if q >= 0 {
						stocks := int(float64(cts.Bet) / q)
						cash -= broker.Buy(stocks, q)
						buys++
						stockss[iCo] = stocks
					} else {
						newDorders = append(newDorders, o)
					}
				}
			}
		}

		if sold {
			safe, cash = withdrawal(cs, nCos, stockss, safe, cash)
		}

		dorders = newDorders

		// Calculate orders //

		for i := 0; i < nCos; i++ {
			q := cs[i]
			if q >= 0 {
				ref := refs[i]
				if toBuys[i] {
					if ref < q {
						dorders = append(dorders, newDailyOrder(false, i, q/ref))
						toBuys[i] = false
					}
				} else if ref > q {
					dorders = append(dorders, newDailyOrder(true, i, 0))
					toBuys[i] = true
				}
			}
		}
	}

	// Main function -------------------------------

	md.fcalc(cls, params, nil, fn)

	for i := 0; i < nCos; i++ {
		stocks := stockss[i]
		if stocks > 0 {
			cash += broker.Sell(stocks, qtable.LastRowOk(cls, i))
		}
	}

	return NewAssetsRs(safe+cash, buys, sells)
}

// Calculates profits ratio of one company (from -1 to ...).
//    opens : (double[days][1]) Open quotes of a company
//            (from before to after).
//    closes: (double[days][1]) Close quotes of a company
//            (from before to after).
//    params: Parameters to calculate.
func (md *T) Profits(opens, closes [][]float64, params []float64) float64 {
	stocks := 0
	cash := float64(cts.InitialCapital)
	toSell := true
	todo := false
	ixOpens := 0

	fn := func(cs []float64, refs []float64) {
		oq := opens[ixOpens][0]
		ixOpens++

		if todo && oq > 0 {
			if toSell { // there is a buy order set in the previous call to fn
				if cash > cts.MinToBet {
					stocks = int(cash / oq)
					cash -= broker.Buy(stocks, oq)
				}
			} else if stocks > 0 {
				cash += broker.Sell(stocks, oq)
				stocks = 0
			}
			todo = false
		}

		q := cs[0]
		if q >= 0 {
			ref := refs[0]
			if toSell {
				if ref > q {
					todo = true
					toSell = false
				}
			} else if ref < q {
				todo = true
				toSell = true
			}
		}
	}

	md.fcalc(closes, params, nil, fn)

	if stocks > 0 {
		cash += broker.Sell(stocks, qtable.LastRowOk(closes, 0))
	}

	return (cash - float64(cts.InitialCapital)) / float64(cts.InitialCapital)
}

// Calculates profits average (see Profits) and standard deviation of all
// the companies of an investor.
//    opens : Open quotes table.
//    closes: Close quotes table.
//    params: Parameters to calculate.
func (md *T) ProfitsAvgSd(
	opens, closes *qtable.T, params []float64,
) (avg float64, sd float64) {
	ops := opens.Values()
	cls := closes.Values()
	nCos := len(ops[0])
	var prfs []float64
	for i := 0; i < nCos; i++ {
		prf := md.Profits(qtable.GetCol(ops, i), qtable.GetCol(cls, i), params)
		prfs = append(prfs, prf)
		avg += prf
	}
	avg /= float64(nCos)
	va := 0.0
	for _, e := range prfs {
		df := e - avg
		va += df * df
	}
	sd = math.Sqrt(va / float64(nCos-1))
	return
}

// Returns references of one company.
//    closes: (double[days][1]) Close quotes of a company
//            (from before to after).
//    params: Parameters to calculate.
//    init  : Initial reference or nil.
func (md *T) Refs(
	closes [][]float64, params []float64, init *refPos.T,
) []float64 {
	r := make([]float64, len(closes))
	ix := 0

	fn := func(cs []float64, refs []float64) {
		r[ix] = refs[0]
		ix++
	}

	md.fcalc(closes, params, init, fn)

	return r
}

// Returns historic assets.
//    opens : Open quotes table.
//    closes: Close quotes table.
//    params: Parameters to calculate.
func (md *T) HistoricAssets(
	opens, closes *qtable.T, params []float64,
) []float64 {
	nCos := len(opens.Nicks())
	ops := opens.Values()
	iops := 0
	cls := closes.Values()

	safe := float64(0)
	cash := float64(cts.InitialCapital)
	var assets []float64

	stockss := make([]int, nCos)
	prices := make([]float64, nCos)
	toBuys := make([]bool, nCos)

	// Auxiliar function ---------------------------

	var dorders []*dailyOrderT
	fn := func(cs []float64, refs []float64) {
		os := ops[iops]
		iops++

		// Execute orders //

		sort.Slice(dorders, func(i, j int) bool {
			o1 := dorders[i]
			o2 := dorders[j]
			if o1.isSell {
				return true
			}
			if o2.isSell {
				return false
			}
			return o1.pond > o2.pond
		})

		var newDorders []*dailyOrderT
		sold := false
		for _, o := range dorders {
			iCo := o.iCo
			if o.isSell {
				stocks := stockss[iCo]
				if stocks > 0 {
					q := os[iCo]
					if q >= 0 {
						cash += broker.Sell(stocks, q)
						stockss[iCo] = 0
						sold = true
					} else {
						newDorders = append(newDorders, o)
					}
				}
			} else {
				if cash > cts.MinToBet {
					q := os[iCo]
					if q >= 0 {
						stocks := int(float64(cts.Bet) / q)
						cash -= broker.Buy(stocks, q)
						stockss[iCo] = stocks
					} else {
						newDorders = append(newDorders, o)
					}
				}
			}
		}

		if sold {
			safe, cash = withdrawal(cs, nCos, stockss, safe, cash)
		}

		dorders = newDorders

		newAssets := cash
		for i, c := range cs {
			q := c
			if c < 0 {
				q = prices[i]
			}
			prices[i] = q
			stocks := stockss[i]
			if stocks > 0 {
				newAssets += broker.Sell(stocks, q)
			}
		}
		assets = append(assets, safe+newAssets)

		// Calculate orders //

		for i := 0; i < nCos; i++ {
			q := cs[i]
			if q >= 0 {
				ref := refs[i]
				if toBuys[i] {
					if ref < q {
						dorders = append(dorders, newDailyOrder(false, i, q/ref))
						toBuys[i] = false
					}
				} else if ref > q {
					dorders = append(dorders, newDailyOrder(true, i, 0))
					toBuys[i] = true
				}
			}
		}
	}

	// Main function -------------------------------

	md.fcalc(cls, params, nil, fn)

	for i := 0; i < nCos; i++ {
		stocks := stockss[i]
		if stocks > 0 {
			cash += broker.Sell(stocks, qtable.LastRowOk(cls, i))
		}
	}

	return append(assets, safe+cash)
}

// Returns orders of a model.
//    dates : Dates of opens and closes.
//    opens : Open quotes table.
//    closes: Close quotes table.
//    params: Parameters to calculate.
func (md *T) Orders(
	dates []string, opens, closes *qtable.T, params []float64,
) []*OrderT {
	var orders []*OrderT
	nicks := opens.Nicks()
	nCos := len(nicks)
	ops := opens.Values()
	cls := closes.Values()

	safe := float64(0)
	cash := cts.InitialCapital
	stockss := make([]int, nCos)
	toBuys := make([]bool, nCos)

	// Auxiliar function ---------------------------

	var dorders []*dailyOrderT
	ix := 0
	fn := func(cs []float64, refs []float64) {
		dt := dates[ix]
		os := ops[ix]
		ix++

		// Execute orders //

		sort.Slice(dorders, func(i, j int) bool {
			o1 := dorders[i]
			o2 := dorders[j]
			if o1.isSell {
				return true
			}
			if o2.isSell {
				return false
			}
			return o1.pond > o2.pond
		})

		var newDorders []*dailyOrderT
		sold := false
		for _, o := range dorders {
			iCo := o.iCo
			if o.isSell {
				stocks := stockss[iCo]
				if stocks > 0 {
					q := os[iCo]
					if q >= 0 {
						cash += broker.Sell(stocks, q)
						stockss[iCo] = 0
						orders = append(orders, NewOrder(
							dt, nicks[iCo], true, stocks, q,
						))
						sold = true
					} else {
						newDorders = append(newDorders, o)
					}
				}
			} else {
				if cash > cts.MinToBet {
					q := os[iCo]
					if q >= 0 {
						stocks := int(float64(cts.Bet) / q)
						cash -= broker.Buy(stocks, q)
						stockss[iCo] = stocks
						orders = append(orders, NewOrder(
							dt, nicks[iCo], false, stocks, q,
						))
					} else {
						newDorders = append(newDorders, o)
					}
				}
			}
		}

		if sold {
			safe, cash = withdrawal(cs, nCos, stockss, safe, cash)
		}

		dorders = newDorders

		// Calculate orders //

		for i := 0; i < nCos; i++ {
			q := cs[i]
			if q >= 0 {
				ref := refs[i]
				if toBuys[i] {
					if ref < q {
						dorders = append(dorders, newDailyOrder(false, i, q/ref))
						toBuys[i] = false
					}
				} else if ref > q {
					dorders = append(dorders, newDailyOrder(true, i, 0))
					toBuys[i] = true
				}
			}
		}
	}

	// Main function -------------------------------

	md.fcalc(cls, params, nil, fn)

	return orders
}
