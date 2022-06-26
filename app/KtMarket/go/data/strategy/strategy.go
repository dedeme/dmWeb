// Copyright 15-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Investor strategy
package strategy

import (
	"github.com/dedeme/KtMarket/cts"
	"github.com/dedeme/KtMarket/data/assetsRs"
	"github.com/dedeme/KtMarket/data/broker"
	"github.com/dedeme/KtMarket/data/model"
	"github.com/dedeme/KtMarket/data/order"
	"github.com/dedeme/KtMarket/data/quote"
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
	return quote.LastValue(Refs(s, closes, initRef))
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

// Returns profits (in percentage of inital capital) of a company
//  s      : Strategy.
//  opens  : Company opens (from before to after), with at least one element.
//  closes : Company closes (from before to after), with at least one element.
func Profits(s *T, opens, closes []float64) float64 {
	acloses := arr.Map(closes, func(c float64) []float64 {
		return []float64{c}
	})
	aopens := arr.Map(opens, func(o float64) []float64 {
		return []float64{o}
	})

	return AllProfits(s, aopens, acloses)
}

// Returns profits (in percentage of inital capital) of a group of companies
//  s      : Strategy.
//  opens  : Companies group opens (from before to after), every one with at
//           least one element.
//  closes : Companies group closes (from before to after), every one with at
//           least one element.
func AllProfits(s *T, opens, closes [][]float64) float64 {
	cash := cts.InitialCapital
	lg := len(opens[0])
	initRefs := make([]float64, lg)
	toSells := make([]bool, lg)
	for i := 0; i < lg; i++ {
		initRefs[i] = -1
		toSells[i] = true
	}
	toDos := make([]bool, lg)
	stockss := make([]int, lg)
	ix := 0

	s.Model.Calc(closes, s.Params, initRefs, func(cs, rs []float64) {
		os := opens[ix]
		ix++

		for i := 0; i < lg; i++ {
			op := os[i]
			cl := cs[i]
			rf := rs[i]
			stocks := stockss[i]
			toSell := toSells[i]
			toDo := toDos[i]

			if toDo && op > 0 {
				if toSell { // there is buy order.
					if cash > cts.MinToBet {
						stocks = int(cash / op)
						stockss[i] = stocks
						cash -= broker.Buy(stocks, op)
					}
				} else if stocks > 0 {
					cash += broker.Sell(stocks, op)
					stockss[i] = 0
				}

				toDos[i] = false
			}

			if cl < 0 {
				continue
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
	})

	for i := 0; i < lg; i++ {
		stocks := stockss[i]
		if stocks > 0 {
			cl := 0.0
			for row := len(closes) - 1; row >= 0; row-- {
				c := closes[row][i]
				if c >= 0 {
					cl = c
					break
				}
			}
			cash += broker.Sell(stocks, cl)
		}
	}

	return (cash - cts.InitialCapital) / cts.InitialCapital
}

// Returns every market order and final results of operating with 'opens' and
// 'closes'.
// Returns profits (in percentage of inital capital) of a group of companies
//  s      : Strategy.
//  dates  : Operation dates. It has the same 'len' that 'closes' and 'opens'.
//  nks    : Nicks matching each column of 'opens' and 'closes'
//  opens  : Companies group opens (from before to after), every one with at
//           least one element.
//  closes : Companies group closes (from before to after), every one with at
//           least one element.
func Orders(
	s *T, dates []string, nks []string, opens, closes [][]float64,
) (orders []*order.T, results *assetsRs.T) {
	buys := 0
	sells := 0
	cash := cts.InitialCapital
	lg := len(opens[0])
	initRefs := make([]float64, lg)
	toSells := make([]bool, lg)
	for i := 0; i < lg; i++ {
		initRefs[i] = -1
		toSells[i] = true
	}
	toDos := make([]bool, lg)
	stockss := make([]int, lg)
	ix := 0

	s.Model.Calc(closes, s.Params, initRefs, func(cs, rs []float64) {
		date := dates[ix]
		os := opens[ix]
		ix++
		for i := 0; i < lg; i++ {
			nk := nks[i]
			op := os[i]
			cl := cs[i]
			rf := rs[i]
			stocks := stockss[i]
			toSell := toSells[i]
			toDo := toDos[i]

			if toDo && op > 0 {
				if toSell { // there is buy order.
					if cash > cts.MinToBet {
						stocks = int(cts.Bet / op)
						stockss[i] = stocks
						cash -= broker.Buy(stocks, op)
						orders = append(orders, order.New(date, nk, false, stocks, op))
						buys++
					}
				} else if stocks > 0 {
					cash += broker.Sell(stocks, op)
					stockss[i] = 0
					orders = append(orders, order.New(date, nk, true, stocks, op))
					sells++
				}

				toDos[i] = false
			}

			if cl < 0 {
				continue
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
	})
	for i := 0; i < lg; i++ {
		stocks := stockss[i]
		if stocks > 0 {
			cl := 0.0
			for row := len(closes) - 1; row >= 0; row-- {
				c := closes[row][i]
				if c >= 0 {
					cl = c
					break
				}
			}
			cash += broker.Sell(stocks, cl)
		}
	}

	results = assetsRs.New(cash, buys, sells)
	return
}

// Diary of assets (one value for each 'open'-'close')
//  s      : Strategy.
//  opens  : Companies group opens (from before to after), every one with at
//           least one element.
//  closes : Companies group closes (from before to after), every one with at
//           least one element.
func Assets(s *T, opens, closes [][]float64) (assets []float64) {
	cash := cts.InitialCapital
	lg := len(opens[0])
	initRefs := make([]float64, lg)
	toSells := make([]bool, lg)
	for i := 0; i < lg; i++ {
		initRefs[i] = -1
		toSells[i] = true
	}
	toDos := make([]bool, lg)
	stockss := make([]int, lg)
	ix := 0

	s.Model.Calc(closes, s.Params, initRefs, func(cs, rs []float64) {
		os := opens[ix]
		ix++
		for i := 0; i < lg; i++ {
			op := os[i]
			cl := cs[i]
			rf := rs[i]
			stocks := stockss[i]
			toSell := toSells[i]
			toDo := toDos[i]

			if toDo && op > 0 {
				if toSell { // there is buy order.
					if cash > cts.MinToBet {
						stocks = int(cts.Bet / op)
						stockss[i] = stocks
						cash -= broker.Buy(stocks, op)
					}
				} else if stocks > 0 {
					cash += broker.Sell(stocks, op)
					stockss[i] = 0
				}

				toDos[i] = false
			}

			if cl < 0 {
				continue
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

		as := cash
		for i := 0; i < lg; i++ {
			stocks := stockss[i]
			if stocks > 0 {
				cl := 0.0
				for row := ix - 1; row >= 0; row-- {
					c := closes[row][i]
					if c >= 0 {
						cl = c
						break
					}
				}
				as += broker.Sell(stocks, cl)
			}
		}

		assets = append(assets, as)
	})

	return
}
