// Copyright 01-Jul-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Model data.
package model

import (
	"github.com/dedeme/KtMMarket/cts"
	"github.com/dedeme/KtMMarket/data/broker"
	"github.com/dedeme/KtMMarket/data/modelEval"
	"github.com/dedeme/KtMMarket/data/order"
	"github.com/dedeme/KtMMarket/data/quotes"
	"github.com/dedeme/KtMMarket/data/result"
	"github.com/dedeme/KtMMarket/data/simProfits"
	"github.com/dedeme/KtMMarket/fns"
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/math"
)

type T struct {
	// Short name.
	Id string
	// Long name.
	Name string
	// Documentation.
	Doc string
	// Parameter names.
	ParamNames []string
	// Minimum param value.
	ParamBases []float64
	// Increment of base steps.
	ParamBaseIncs []float64
	// Increment of step environment.
	ParamEnvInc []float64
	// Function to calculate operations.
	//    closes: Normalized closes in matrix 'dates x cos' ordered from before to after.
	//    params: Values to calculate.
	//    action: Function called after calculate 'refs'
	//            When closes[i] >= refs[i], position is bought.
	//            When closes[i] < refs[i], position is sold.
	//            Params:
	//              closes: Last closes (without -1). One for each company.
	//              refs  : Last references. One for each company.
	Calc func(
		closes [][]float64,
		params []float64,
		action func(closes []float64, refs []float64))
}

// Returns results of run a simulation with 'qs':
//  orders: Every market order ordered by date.
//  hassets: Assets historic. One value for each 'qs.Dates'.
//  hwithdrawal: Withdrawal historic. One value for each 'qs.Dates'.
//  cash: Final cash + withdrawals.
//  refAssets: Final assets using references intead closes (risk).
//  refs: Referencies. One for each 'qs.Closes'
//  buys: Buy dates. A slice for each 'qs.Cos'
//  sales: Sales dates. A slice for each 'qs.Cos'
//  profits: Profits ratios. One for each 'qs.Cos'
// Parameters
//  quotes: Quotes for simulation.
func (md *T) Simulation(qs *quotes.T, params []float64) (
	orders []*order.T, hassets, hwithdrawal []float64, cash, refAssets float64,
	refs [][]float64, buys, sales [][]string, profits []float64,
) {
	dates := qs.Dates
	nDates := len(dates)
	nks := qs.Cos
	nCos := len(qs.Cos)
	opens := qs.Opens
	closes := qs.Closes
	maxs := qs.Maxs
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
	for i := 0; i < nCos; i++ {
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

	md.Calc(closes, params, func(cs, rs []float64) {
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
    if total > cts.InitialCapital + cts.Bet + cts.Bet {
      dif := total - cts.InitialCapital - cts.Bet
      securAmount := cts.MinToBet - cts.Bet
      withdraw := -1.0
      if cashIn > dif + securAmount {
        withdraw = dif
      } else if cashIn > cts.MinToBet {
        withdraw = math.Floor((cashIn - securAmount) / cts.Bet) * cts.Bet
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

func (md *T) groupEvaluation(qs *quotes.T, params []float64) (
	rs *result.T, profs *simProfits.T,
) {
	rs = result.New(0.0, 0.0, 0.0)
	profs = simProfits.New(0.0, 0.0, 0.0)
	resultsN := 0

	steps := cts.EnvSteps
	totalSteps := steps + steps + 1
	incs := md.ParamEnvInc
	nparams := len(params)
	ixparams := 0
	var pms []float64
	var paramIxs []int
	for {
		if len(paramIxs) <= ixparams {
			paramIxs = append(paramIxs, 0)
			pms = append(pms, params[ixparams]+incs[ixparams]*float64(-steps))
		} else {
			ixval := paramIxs[ixparams] + 1
			if ixval >= totalSteps {
				if ixparams == 0 {
					break
				}
				paramIxs[ixparams] = -1
				ixparams--
				continue
			}
			paramIxs[ixparams] = ixval
			pms[ixparams] = params[ixparams] +
				incs[ixparams]*float64(ixval-steps)
		}

		ixparams++
		if ixparams < nparams {
			continue
		}

		if ixparams == nparams {
			orders, hassets, _, cash, refAssets, _, _, _, profits :=
				md.Simulation(qs, pms)
			assets := hassets[len(hassets)-1]

			newRs := result.New(
				assets,
				arr.Reduce(profits, 0, func(r, e float64) float64 {
					return r + e
				})/float64(len(profits)),
				float64(order.Sales(orders)),
			)
			rs = rs.Sum(newRs)

			newProfs := simProfits.New(assets, cash, refAssets)
			profs = profs.Sum(newProfs)

			resultsN++
		}

		ixparams--
	}

	return rs.Div(resultsN), profs.Div(resultsN)
}

func (md *T) rangeEvaluation(
	qs *quotes.T, evals []*modelEval.T, profits []*simProfits.RowT, isNew bool,
) (newEvals []*modelEval.T, newProfits []*simProfits.RowT) {
	bases := md.ParamBases
	incs := md.ParamBaseIncs
	nparams := len(bases)
	ixparams := 0
	var params []float64
	var paramIxs []int
	for {
		if len(paramIxs) <= ixparams {
			paramIxs = append(paramIxs, 0)
			params = append(params, md.ParamBases[ixparams])
		} else {
			ixval := paramIxs[ixparams] + 1
			if ixval >= cts.EvalSteps {
				if ixparams == 0 {
					break
				}
				paramIxs[ixparams] = -1
				params[ixparams] = bases[ixparams] - incs[ixparams]
				ixparams--
				continue
			}
			paramIxs[ixparams] = ixval
			params[ixparams] += incs[ixparams]
		}

		ixparams++
		if ixparams < nparams {
			continue
		}

		mdEv, ok := arr.Find(evals, func(me *modelEval.T) bool {
			return fns.EqParams(me.Params, params)
		})
		if !ok {
			mdEv = modelEval.New(params, 0, 0.0, 0.0, 0.0, 0.0)
		}

		profsRow, ok := arr.Find(profits, func(p *simProfits.RowT) bool {
			return fns.EqParams(p.Params, params)
		})
		if !ok {
			profsRow = simProfits.NewRow(
				params,
				0,
				simProfits.New(0.0, 0.0, 0.0),
				simProfits.New(0.0, 0.0, 0.0),
			)
		}

		rs, profs := md.groupEvaluation(qs, params)

		n := float64(mdEv.Weeks)
		n1 := n + 1
		value := rs.Eval()
		sales := rs.Sales

		var newMdEv *modelEval.T
		if isNew || n == 0 {
			newN := mdEv.Weeks
			if newN < cts.EvalWeeks {
				newN += 1
			}
			newMdEv = modelEval.New(
				fns.CopyParams(params),
				newN,
				(mdEv.Hvalue*n+value)/n1,
				(mdEv.Hsales*n+sales)/n1,
				value,
				sales,
			)
		} else {
			newMdEv = modelEval.New(
				fns.CopyParams(params),
				mdEv.Weeks,
				(mdEv.Hvalue*n-mdEv.Value+value)/n,
				(mdEv.Hsales*n-mdEv.Sales+sales)/n,
				value,
				sales,
			)
		}

		newEvals = append(newEvals, newMdEv)

		n = float64(profsRow.Weeks)
		n1 = n + 1
		total := profsRow.Hprofits.Total
		cash := profsRow.Hprofits.Cash
		ref := profsRow.Hprofits.Ref

		var newProfsRow *simProfits.RowT
		if isNew || n == 0 {
			newN := mdEv.Weeks
			if newN < cts.EvalWeeks {
				newN += 1
			}
			newProfsRow = simProfits.NewRow(
				fns.CopyParams(params),
				newN,
				simProfits.New(
					(total*n+profs.Total)/n1,
					(cash*n+profs.Cash)/n1,
					(ref*n+profs.Ref)/n1,
				),
				profs,
			)
		} else {
			newProfsRow = simProfits.NewRow(
				fns.CopyParams(params),
				profsRow.Weeks,
				simProfits.New(
					(total*n-profsRow.Profits.Total+profs.Total)/n,
					(cash*n-profsRow.Profits.Cash+profs.Cash)/n,
					(ref*n-profsRow.Profits.Ref+profs.Ref)/n,
				),
				profs,
			)
		}

		newProfits = append(newProfits, newProfsRow)

		ixparams--
	}

	return
}

// Reevaluate 'evals' in a new 'modelEval.T', adding a new evaluation.
func (md *T) RangeNewSimulation(
	qs *quotes.T, evals []*modelEval.T, profits []*simProfits.RowT,
) (newEvals []*modelEval.T, newProfits []*simProfits.RowT) {
	return md.rangeEvaluation(qs, evals, profits, true)
}

// Reevaluate 'evals' in a new 'modelEval.T', replacing the last evaluation.
func (md *T) RangeReplaceSimulation(
	qs *quotes.T, evals []*modelEval.T, profits []*simProfits.RowT,
) (newEvals []*modelEval.T, newProfits []*simProfits.RowT) {
	return md.rangeEvaluation(qs, evals, profits, false)
}

// NOTE: Parameter m.Calc is not serialized.
func ToJs(m *T) string {
	return js.Wa([]string{
		js.Ws(m.Id),
		js.Ws(m.Name),
		js.Ws(m.Doc),
		js.Wa(arr.Map(m.ParamNames, js.Ws)),
		js.Wa(arr.Map(m.ParamBases, js.Wd)),
		js.Wa(arr.Map(m.ParamBaseIncs, js.Wd)),
		js.Wa(arr.Map(m.ParamEnvInc, js.Wd)),
	})
}

// NOTE: 'j' was serialized with 'ToJs'.
//
// For finding the model the 'id' returned is searched in "List()' (list.go).
// If no model is found, panic is raised.
func FromJs(j string) *T {
	a := js.Ra(j)
	id := js.Rs(a[0])
	md, ok := arr.Find(List(), func(m *T) bool {
		return m.Id == id
	})
	if !ok {
		panic("Model '" + id + "' not found")
	}
	return md
}

// Serializes only 'm.Id' for using with tables.
func ToJsTb(m *T) string {
	return js.Ws(m.Id)
}

// Deserializes a model serialized with 'ToJsTb'
//
// For finding the model the 'id' returned is searched in "List()' (list.go).
// If no model is found, panic is raised.
func FromJsTb(j string) *T {
	id := js.Rs(j)
	md, ok := arr.Find(List(), func(m *T) bool {
		return m.Id == id
	})
	if !ok {
		panic("Model '" + id + "' not found")
	}
	return md
}
