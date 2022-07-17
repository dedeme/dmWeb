// Copyright 15-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Investor strategy
package strategy

import (
	"github.com/dedeme/KtMarket/cts"
	"github.com/dedeme/KtMarket/data/broker"
	"github.com/dedeme/KtMarket/data/model"
	"github.com/dedeme/KtMarket/data/order"
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/math"
)

type T struct {
	// Investing model
	Model *model.T
	// Model parameters
	Params []float64
}

func New(md *model.T, params []float64) *T {
	return &T{md, params}
}

func Eq(st1, st2 *T) bool {
	if st1.Model.Id != st2.Model.Id {
		return false
	}
	for i := 0; i < len(st1.Params); i++ {
		if !math.Eq(st1.Params[i], st2.Params[i], 0.0000001) {
			return false
		}
	}
	return true
}

func ToJs(s *T) string {
	return js.Wa([]string{
		model.ToJs(s.Model),
		js.Wa(arr.Map(s.Params, js.Wd)),
	})
}

// Serialization only for using with tables in connection with 'FromJsTb'
func ToJsTb(s *T) string {
	return js.Wa([]string{
		model.ToJsTb(s.Model),
		js.Wa(arr.Map(s.Params, js.Wd)),
	})
}

// Deserializes a model serialized with 'ToJsTb'
func FromJsTb(j string) *T {
	a := js.Ra(j)
	return New(
		model.FromJsTb(a[0]),
		arr.Map(js.Ra(a[1]), js.Rd),
	)
}

// Returns references (one for each close) of one company
//  s      : Strategy.
//  closes : Company closes (from before to after), with at least one element.
//  initRef: Initial reference or -1 if there is no one.
func Refs(s *T, closes []float64, initRef float64) []float64 {
	var r []float64

	acloses := arr.Map(closes, func(c float64) []float64 {
		return []float64{c}
	})
	s.Model.Calc(acloses, s.Params, []float64{initRef}, func(cs, rs []float64) {
		r = append(r, rs[0])
	})
	return r
}

// Returns the last reference of one company
//  s      : Strategy.
//  closes : Company closes (from before to after), with at least one element.
//  initRef: Initial reference or -1 if there is no one.
func LastRef(s *T, closes []float64, initRef float64) float64 {
	r := Refs(s, closes, initRef)
	return r[len(r)-1]
}

// Returns references (one for each close) of a group of companies
//  s      : Strategy.
//  closes : Companies group closes (from before to after), every one with at
//           least one element.
//  initRef: Initial reference for each company or -1 if there is no one.
func AllRefs(s *T, closes [][]float64, initRefs []float64) [][]float64 {
	var r [][]float64

	s.Model.Calc(closes, s.Params, initRefs, func(cs, rs []float64) {
		r = append(r, rs)
	})
	return r
}

// Returns results of run a simulation:
//  orders: Every market order ordered by date.
//  hassets: Assets historic. One value for each 'qs.Dates'.
//  hwithdrawal: Withdrawal historic. One value for each 'qs.Dates'.
//  cash: Final cash.
//  refAssets: Final assets using references intead closes (risk).
//  refs: Referencies. One for each 'qs.Closes'
//  buys: Buy dates. A slice for each 'qs.Cos'
//  sales: Sales dates. A slice for each 'qs.Cos'
//  profits: Profits ratios. One for each 'qs.Cos'
// Parameters
//  s     : Stragegy
//  dates : Dates of opens (for before to after).
//  nks   : Ordered nicks (one for each column of opens)
//  opens : Matrix of 'dates x companies' with historic opens.
//  closes: closes matching 'opens'.
//  maxs  : maximums matching 'opens'.
func Simulation(
	s *T, dates []string, nks []string, opens, closes, maxs [][]float64,
) (orders []*order.T, hassets, hwithdrawal []float64, cash, refAssets float64,
	refs [][]float64, buys, sales [][]string, profits []float64,
) {
	nDates := len(dates)
	nCos := len(nks)
	cashIn := cts.InitialCapital
	withdrawal := 0.0

	hassets = make([]float64, nDates)
	hwithdrawal = make([]float64, nDates)
	refs = make([][]float64, nDates)
	for i := range dates {
		refs[i] = make([]float64, nCos)
	}
	buys = make([][]string, nCos)
	sales = make([][]string, nCos)
	profits = make([]float64, nCos)

	prfCashs := make([]float64, nCos)
	toSells := make([]bool, nCos)
	initRefs := make([]float64, nCos)
	for i := 0; i < nCos; i++ {
		initRefs[i] = -1.0
		toSells[i] = true
		prfCashs[i] = cts.Bet
	}
	toDos := make([]bool, nCos)
	stockss := make([]int, nCos)
	prfStockss := make([]int, nCos)
	prices := make([]float64, nCos)
	prfPrices := make([]float64, nCos)
	coughts := make([]bool, nCos)
	prfCoughts := make([]bool, nCos)
	ix := 0

	s.Model.Calc(closes, s.Params, initRefs, func(cs, rs []float64) {
		refs[ix] = rs
		date := dates[ix]
		os := opens[ix]
		mxs := maxs[ix]

		assets := 0.0
		for i, nk := range nks {
			op := os[i]
			cl := cs[i]
			rf := rs[i]
			toSell := toSells[i]
			toDo := toDos[i]

			if toDo {
				if toSell { // there is buy order.
					if !prfCoughts[i] {
						prfCash := prfCashs[i]
						stocks := int((prfCash - broker.Fees(prfCash)) / op)
						cost := broker.Buy(stocks, op)
						for cost > prfCash {
							stocks--
							cost = broker.Buy(stocks, op)
						}
						prfStockss[i] = stocks
						prfCashs[i] -= cost
						buys[i] = append(buys[i], date)
						prfPrices[i] = op
					}
					if cashIn > cts.MinToBet && !coughts[i] {
						stocks := int(cts.Bet / op)
						stockss[i] = stocks
						cashIn -= broker.Buy(stocks, op)
						orders = append(orders, order.New(date, nk, order.BUY, stocks, op))
						prices[i] = op
					}
				} else {
					stocks := stockss[i]
					if stocks > 0 && !coughts[i] {
						if op > prices[i]*cts.NoLostMultiplicator {
							cashIn += broker.Sell(stocks, op)
							stockss[i] = 0
							orders = append(orders, order.New(date, nk, order.SELL, stocks, op))
						} else {
							orders = append(orders, order.New(date, nk, order.CATCH, stocks, op))
							coughts[i] = true
						}
					}
					stocks = prfStockss[i]
					if stocks > 0 && !prfCoughts[i] {
						if op > prfPrices[i]*cts.NoLostMultiplicator {
							prfCashs[i] += broker.Sell(stocks, op)
							prfStockss[i] = 0
							sales[i] = append(sales[i], date)
						} else {
							prfCoughts[i] = true
						}
					}
				}

				toDos[i] = false
			}

			if coughts[i] {
				price := prices[i] * cts.NoLostMultiplicator
				if mxs[i] > price {
					stocks := stockss[i]
					cashIn += broker.Sell(stocks, price)
					stockss[i] = 0
					orders = append(orders, order.New(date, nk, order.SELL, stocks, price))
					coughts[i] = false
				}
			}

			if prfCoughts[i] {
				price := prfPrices[i] * cts.NoLostMultiplicator
				if mxs[i] > price {
					stocks := prfStockss[i]
					prfCashs[i] += broker.Sell(stocks, price)
					prfStockss[i] = 0
					sales[i] = append(sales[i], date)
					prfCoughts[i] = false
				}
			}

			stks := stockss[i]
			if stks > 0 {
				assets += broker.Sell(stks, cl)
			}

			if toSell {
				if rf > cl {
					toDos[i] = true
					toSells[i] = false
				}
			} else if rf < cl {
				toDos[i] = true
				toSells[i] = true
			}
		}

		total := cashIn + assets
		if total > cts.InitialCapital+cts.Bet+cts.Bet {
			dif := total - cts.InitialCapital - cts.Bet
			securAmount := cts.MinToBet - cts.Bet
			withdraw := -1.0
			if cashIn > dif+securAmount {
				withdraw = dif
			} else if cashIn > cts.MinToBet {
				withdraw = math.Floor((cashIn-securAmount)/cts.Bet) * cts.Bet
			}
			if withdraw > 0 {
				withdrawal += withdraw
				cashIn -= withdraw
			}
		}

		hassets[ix] = cashIn + withdrawal + assets
		hwithdrawal[ix] = withdrawal
		ix++
	})

	cash = cashIn + withdrawal
	refAss := 0.0
	lastCloses := closes[nDates-1]
	lastRef := refs[nDates-1]
	for i := range nks {
		stks := prfStockss[i]
		if stks == 0 {
			profits[i] = (prfCashs[i] - cts.Bet) / cts.Bet
		} else {
			profits[i] = (prfCashs[i] - cts.Bet + broker.Sell(stks, lastCloses[i])) /
				cts.Bet
		}
		stks = stockss[i]
		if stks > 0 {
			refAss += broker.Sell(stks, lastRef[i])
		}
	}
	refAssets = cash + refAss

	return
}
