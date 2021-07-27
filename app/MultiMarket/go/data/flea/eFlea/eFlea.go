// Copyright 11-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Evaluated flea data.
package eFlea

import (
	"github.com/dedeme/MultiMarket/data/cts"
	"github.com/dedeme/MultiMarket/data/flea"
	"github.com/dedeme/MultiMarket/data/flea/fmodel"
	"github.com/dedeme/MultiMarket/data/qtable"
	"github.com/dedeme/MultiMarket/global/fn"
	"github.com/dedeme/golib/json"
	"sort"
)

type T struct {
	flea       *flea.T
	buys       int
	sells      int
	assets     float64
	profitsAvg float64
	profitsSd  float64
	// Point results of evaluation. If its value is less than 0, the flea has not
	// been evaluated.
	Eval          float64
	HistoricSales float64
	HistoricEval  float64
}

func New(f *flea.T) *T {
	return &T{f, 0, 0, 0.0, 0.0, 0.0, -1.0, -1.0, -1.0}
}

// Evaluate flea
func (e *T) Flea() *flea.T {
	return e.flea
}

// Buys number
func (e *T) Buys() int {
	return e.buys
}

// Sells number
func (e *T) Sells() int {
	return e.sells
}

// Assets (money) earned.
func (e *T) Assets() float64 {
	return e.assets
}

// Profits average (ratio) earned.
func (e *T) ProfitsAvg() float64 {
	return e.profitsAvg
}

// Standard deviation of profits distribution between companies.
func (e *T) ProfitsSd() float64 {
	return e.profitsSd
}

// Two efleas are equals if they have same values buys, sells, assets,
// profitsAvg and profitsStd.
func (e *T) Eq(e2 *T) (ok bool) {
	if e.flea.Eq(e2.flea) ||
		(e.buys == e2.buys &&
			e.sells == e2.sells &&
			fn.Eq(e.assets, e2.assets, 0.00000001) &&
			fn.Eq(e.profitsAvg, e2.profitsAvg, 0.00000001) &&
			fn.Eq(e.profitsSd, e2.profitsSd, 0.00000001)) {
		ok = true
	}
	if !ok && e.flea.Eq(e2.flea) {

		panic("Eval: " + e.flea.ToJs() + "!=" + e2.flea.ToJs() + "\n" +
			e.ToJs() + "\n" + e2.ToJs())
	}
	return
}

// Returns true if "es" contains "e". Compare only 'e.flea' with 'flea.Eq'.
//    es    : Evaluated fleas list.
func (e *T) IsIn(es []*T) bool {
	for _, e2 := range es {
		if e.Eq(e2) {
			return true
		}
	}
	return false
}

// Updates in place 'e'.
//    buys      : Buys number.
//    sells     : Sells number.
//    assets    : Assets (money) earned.
//    profitsAvg: Profits average (ratio) earned.
//    profitsSd : Standard deviation of profits distribution between companies.
func (e *T) Update(buys, sells int, assets, profitsAvg, profitsSd float64) {
	e.buys = buys
	e.sells = sells
	e.assets = assets
	e.profitsAvg = profitsAvg
	e.profitsSd = profitsSd
}

func (e *T) ToJs() json.T {
	return json.Wa([]json.T{
		e.flea.ToJs(),
		json.Wi(e.buys),
		json.Wi(e.sells),
		json.Wd(e.assets),
		json.Wd(e.profitsAvg),
		json.Wd(e.profitsSd),
		json.Wd(e.Eval),
		json.Wd(e.HistoricSales),
		json.Wd(e.HistoricEval),
	})
}

func FromJs(js json.T) *T {
	a := js.Ra()
	return &T{
		flea.FromJs(a[0]),
		a[1].Ri(),
		a[2].Ri(),
		a[3].Rd(),
		a[4].Rd(),
		a[5].Rd(),
		a[6].Rd(),
		a[7].Rd(),
		a[8].Rd(),
	}
}

// Reevaluates in place a list of evaluated fleas.
//    md    : Flea model.
//    opens : Open quotes table.
//    closes: Close quotes table.
//    es    : Evaluated fleas list.
func Evaluate(md *fmodel.T, opens, closes *qtable.T, es []*T) {
	for _, e := range es {
		rs := md.Assets(opens, closes, e.flea.Param())
		e.buys = rs.Buys()
		e.sells = rs.Sells()
		e.assets = rs.Assets()

		avg, sd := md.ProfitsAvgSd(opens, closes, e.flea.Param())
		e.profitsAvg = avg
		e.profitsSd = sd
	}

	for _, e := range es {
		e.Eval = e.flea.Evaluate(
			e.assets/cts.InitialCapital,
			e.profitsAvg+1,
			// profitsAvs = Avg of [(cash - cts.InitialCapital) / cts.InitialCapital]
			// e.profitsAvg+1 = Avg of [cash / cts.InitialCapital]
			e.profitsSd,
		)
	}
}

// Sorts a evaluated flea list for better to worse.
//    es    : Evaluated fleas list.
func Sort(es []*T) {
	sort.Slice(es, func(i, j int) bool {
		return es[i].Eval > es[j].Eval
	})
}

// Remove duplicates (Compare only 'e.flea' with 'flea.Eq'), returning a new
// list.
//    es    : Evaluated fleas list.
func RemoveDuplicates(es []*T) []*T {
	var r []*T
	for _, e := range es {
		if !e.IsIn(r) {
			r = append(r, e)
		}
	}
	return r
}

// Adds elements from source to target, starting by the begining of source.
// The result is a new not ordered slice.
//
// Elements duplicated in "target" are no added.
//
// If target reaches the length of "n", the process is stopped.
//    source: Evaluated fleas to add.
//   target : Evaluated fleas target.
//   n      : Maximum length allowed of target.
func Complete(source, target []*T, n int) []*T {
	for _, e := range source {
		if len(target) >= n {
			break
		}
		if !e.IsIn(target) {
			target = append(target, e)
		}
	}
	return target
}
