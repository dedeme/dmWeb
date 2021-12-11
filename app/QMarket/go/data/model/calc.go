// Copyright 05-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Bet system
package model

import (
	"github.com/dedeme/QMarket/data/brokerM"
	"github.com/dedeme/QMarket/data/cts"
	"github.com/dedeme/QMarket/data/model/initialOrder"
	"github.com/dedeme/QMarket/data/model/management"
	"github.com/dedeme/QMarket/data/model/order"
	"github.com/dedeme/QMarket/data/qtable"
	"sort"
)

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
				tmpAssets += brokerM.Sell(stocks, q)
			}
		}
	}
	if tmpAssets > cts.InitialCapital+cts.Bet+cts.Bet {
		dif := tmpAssets - cts.InitialCapital - cts.Bet
		if cash >= dif+1000 {
			safe += dif
			cash -= dif
		} else if cash >= cts.Bet+1000 {
			dif = float64(int((cash-1000)/cts.Bet)) * cts.Bet
			safe += dif
			cash -= dif
		}
	}
	return safe, cash
}

// Calculates assets of a investor by simulation of real operations.
//    opens : Open quotes table.
//    closes: Close quotes table.
//    ------
//    Return:
//      buys: Number of buys.
//      sales: Number of sales.
//      assets: Final assets (€), from 0 to ...
func (m *T) Assets(opens, closes *qtable.T) (
	buys, sales int, assets float64,
) {
	nCos := len(opens.Nicks())
	ops := opens.Values()
	iops := 0
	cls := closes.Values()

	safe := float64(0)
	cash := float64(cts.InitialCapital)

	stockss := make([]int, nCos)
	toBuys := make([]bool, nCos)

	// Auxiliar function ---------------------------

	var dorders []*initialOrder.T
	fn := func(cs []float64, refs []float64) {
		os := ops[iops]
		iops++

		// Execute orders //

		sort.Slice(dorders, func(i, j int) bool {
			o1 := dorders[i]
			o2 := dorders[j]
			if o1.IsSale() {
				return true
			}
			if o2.IsSale() {
				return false
			}
			return o1.Pond() > o2.Pond()
		})

		var newDorders []*initialOrder.T
		sold := false
		for _, o := range dorders {
			iCo := o.ICo()
			if o.IsSale() {
				stocks := stockss[iCo]
				if stocks > 0 {
					q := os[iCo]
					if q >= 0 {
						cash += brokerM.Sell(stocks, q)
						sales++
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
						cash -= brokerM.Buy(stocks, q)
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
						dorders = append(dorders, initialOrder.New(false, i, q/ref))
						toBuys[i] = false
					}
				} else if ref > q {
					dorders = append(dorders, initialOrder.New(true, i, 0))
					toBuys[i] = true
				}
			}
		}
	}

	// Main function -------------------------------

	management.Run(cls, m.qlevel, m.param, fn)

	for i := 0; i < nCos; i++ {
		stocks := stockss[i]
		if stocks > 0 {
			cash += brokerM.Sell(stocks, qtable.LastRowOk(cls, i))
		}
	}

	assets = safe + cash
	return
}

// Calculates profits ratio of one company (from -1 to ...).
//    opens : (double[days][1]) Open quotes of a company
//            (from before to after).
//    closes: (double[days][1]) Close quotes of a company
//            (from before to after).
func (m *T) Profits(opens, closes [][]float64) float64 {
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
					cash -= brokerM.Buy(stocks, oq)
				}
			} else if stocks > 0 {
				cash += brokerM.Sell(stocks, oq)
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

	management.Run(closes, m.qlevel, m.param, fn)

	if stocks > 0 {
		cash += brokerM.Sell(stocks, qtable.LastRowOk(closes, 0))
	}

	return (cash - float64(cts.InitialCapital)) / float64(cts.InitialCapital)
}

// Calculates profits average (see Profits) of all the companies.
//    opens : Open quotes table.
//    closes: Close quotes table.
func (m *T) ProfitsAvg(opens, closes *qtable.T) float64 {
	ops := opens.Values()
	cls := closes.Values()
	nCos := len(ops[0])
	sum := 0.0
	for i := 0; i < nCos; i++ {
		sum += m.Profits(qtable.GetCol(ops, i), qtable.GetCol(cls, i))
	}
	return sum / float64(nCos)
}

// Returns references of one company.
//    closes: (double[days][1]) Close quotes of a company
//            (from before to after).
func (m *T) Refs(closes [][]float64) []float64 {
	r := make([]float64, len(closes))
	ix := 0

	fn := func(cs []float64, refs []float64) {
		r[ix] = refs[0]
		ix++
	}

	management.Run(closes, m.qlevel, m.param, fn)

	return r
}

// Returns 'true' if the current position of 'm' is 'toSell'.
//    closes: (double[days][1]) Close quotes of a company
//            (from before to after).
func (m *T) IsToSell(closes [][]float64) bool {
	toSell := true
	fn := func(cs []float64, refs []float64) {
		q := cs[0]
		if q >= 0 {
			ref := refs[0]
			if toSell {
				if ref > q {
					toSell = false
				}
			} else if ref < q {
				toSell = true
			}
		}
	}

	management.Run(closes, m.qlevel, m.param, fn)

	return toSell
}

// Returns historic assets.
//    opens : Open quotes table.
//    closes: Close quotes table.
func (m *T) HistoricAssets(opens, closes *qtable.T) []float64 {
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

	var dorders []*initialOrder.T
	fn := func(cs []float64, refs []float64) {
		os := ops[iops]
		iops++

		// Execute orders //

		sort.Slice(dorders, func(i, j int) bool {
			o1 := dorders[i]
			o2 := dorders[j]
			if o1.IsSale() {
				return true
			}
			if o2.IsSale() {
				return false
			}
			return o1.Pond() > o2.Pond()
		})

		var newDorders []*initialOrder.T
		sold := false
		for _, o := range dorders {
			iCo := o.ICo()
			if o.IsSale() {
				stocks := stockss[iCo]
				if stocks > 0 {
					q := os[iCo]
					if q >= 0 {
						cash += brokerM.Sell(stocks, q)
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
						cash -= brokerM.Buy(stocks, q)
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
				newAssets += brokerM.Sell(stocks, q)
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
						dorders = append(dorders, initialOrder.New(false, i, q/ref))
						toBuys[i] = false
					}
				} else if ref > q {
					dorders = append(dorders, initialOrder.New(true, i, 0))
					toBuys[i] = true
				}
			}
		}
	}

	// Main function -------------------------------

	management.Run(cls, m.qlevel, m.param, fn)

	for i := 0; i < nCos; i++ {
		stocks := stockss[i]
		if stocks > 0 {
			cash += brokerM.Sell(stocks, qtable.LastRowOk(cls, i))
		}
	}

	return append(assets, safe+cash)
}

// Returns orders of a model.
//    dates : Dates of opens and closes.
//    opens : Open quotes table.
//    closes: Close quotes table.
func (m *T) Orders(
	dates []string, opens, closes *qtable.T,
) []*order.T {
	var orders []*order.T
	nicks := opens.Nicks()
	nCos := len(nicks)
	ops := opens.Values()
	cls := closes.Values()

	safe := float64(0)
	cash := cts.InitialCapital
	stockss := make([]int, nCos)
	toBuys := make([]bool, nCos)

	// Auxiliar function ---------------------------

	var dorders []*initialOrder.T
	ix := 0
	fn := func(cs []float64, refs []float64) {
		dt := dates[ix]
		os := ops[ix]
		ix++

		// Execute orders //

		sort.Slice(dorders, func(i, j int) bool {
			o1 := dorders[i]
			o2 := dorders[j]
			if o1.IsSale() {
				return true
			}
			if o2.IsSale() {
				return false
			}
			return o1.Pond() > o2.Pond()
		})

		var newDorders []*initialOrder.T
		sold := false
		for _, o := range dorders {
			iCo := o.ICo()
			if o.IsSale() {
				stocks := stockss[iCo]
				if stocks > 0 {
					q := os[iCo]
					if q >= 0 {
						cash += brokerM.Sell(stocks, q)
						stockss[iCo] = 0
						orders = append(orders, order.New(
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
						cash -= brokerM.Buy(stocks, q)
						stockss[iCo] = stocks
						orders = append(orders, order.New(
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
						dorders = append(dorders, initialOrder.New(false, i, q/ref))
						toBuys[i] = false
					}
				} else if ref > q {
					dorders = append(dorders, initialOrder.New(true, i, 0))
					toBuys[i] = true
				}
			}
		}
	}

	// Main function -------------------------------

	management.Run(cls, m.qlevel, m.param, fn)

	return orders
}
