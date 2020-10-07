// Copyright 13-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Investor data.
package investor

import (
	"github.com/dedeme/MultiMarket/data/flea/eval"
	"github.com/dedeme/MultiMarket/data/flea/fmodel"
	"github.com/dedeme/MultiMarket/data/flea/fmodels"
	"github.com/dedeme/MultiMarket/data/qtable"
	"github.com/dedeme/golib/json"
	"sort"
)

type T struct {
	model *fmodel.T
	eflea *eval.T
}

func New(model *fmodel.T, eflea *eval.T) *T {
	return &T{model, eflea}
}

func (i *T) Model() *fmodel.T {
	return i.model
}

func (i *T) Eflea() *eval.T {
	return i.eflea
}

// Returns true if "es" contains "e". Compare only 'e.flea' with 'flea.Eq'.
//    es    : Evaluated fleas list.
func (i *T) IsIn(is []*T) bool {
	for _, e := range is {
		if i.eflea.Flea().Eq(e.eflea.Flea()) && i.model.Id() == e.model.Id() {
			return true
		}
	}
	return false
}

// Returns an investor JSONized to save in file system.
func (i *T) ToJs() json.T {
	return json.Wa([]json.T{
		json.Ws(i.model.Id()),
		i.eflea.ToJs(),
	})
}

// Returns an investor JSONized for sending to client (javascript).
func (i *T) ToJsClient() json.T {
	return json.Wa([]json.T{
		i.model.ToJs(),
		i.eflea.ToJs(),
	})
}

// Returns an investor from a JSON value generated with 'tojs'. If the model is
// not found, returns 'ok==false'.
func FromJs(js json.T) (inv *T, ok bool) {
	a := js.Ra()
	md, ok := fmodels.GetModel(a[0].Rs())
	if ok {
		inv = &T{
			md,
			eval.FromJs(a[1]),
		}
	}
	return
}

// Returns a list of investors from a serialized list of them, removing
// those that can not be deserialized.
//    jss: List of 'JSONized' investors.
func FromJsList(jss []json.T) (invs []*T) {
	for _, e := range jss {
		inv, ok := FromJs(e)
		if ok {
			invs = append(invs, inv)
		}
	}
	return
}

// Reevaluates in place a list of investors.
//    opens : Open quotes table.
//    closes: Close quotes table.
//    invs  : Investors list.
func Evaluate(opens, closes *qtable.T, invs []*T) {
	minAssets := 2000000.0
	maxAssets := -2000000.0
	minProfitsAvg := 2000.0
	maxProfitsAvg := -2000.0
	minProfitsVa := 2000.0
	maxProfitsVa := -2000.0
	for _, inv := range invs {
		md := inv.model
		e := inv.eflea
		rs := md.Assets(opens, closes, e.Flea().Params())
		avg, va := md.ProfitsAvgVa(opens, closes, e.Flea().Params())
		e.Update(rs.Buys(), rs.Sells(), rs.Assets(), avg, va)

		assets := rs.Assets()
		profitsAvg := avg
		profitsVa := va
		if assets < minAssets {
			minAssets = assets
		}
		if assets > maxAssets {
			maxAssets = assets
		}
		if profitsAvg < minProfitsAvg {
			minProfitsAvg = profitsAvg
		}
		if profitsAvg > maxProfitsAvg {
			maxProfitsAvg = profitsAvg
		}
		if profitsVa < minProfitsVa {
			minProfitsVa = profitsVa
		}
		if profitsVa > maxProfitsVa {
			maxProfitsVa = profitsVa
		}
	}

	assetsDf := maxAssets - minAssets
	profitsAvgDf := maxProfitsAvg - minProfitsAvg
	profitsVaDf := maxProfitsVa - minProfitsVa

	for _, inv := range invs {
		e := inv.eflea
		e.Eval = e.Flea().Evaluate(
			(e.Assets()-minAssets)/assetsDf,
			(e.ProfitsAvg()-minProfitsAvg)/profitsAvgDf,
			(e.ProfitsVa()-minProfitsVa)/profitsVaDf,
		)
	}
}

// Sorts a investord list for better to worse.
//    invs: investor list.
func Sort(invs []*T) {
	sort.Slice(invs, func(i, j int) bool {
		return invs[i].eflea.Eval > invs[j].eflea.Eval
	})
}

// Remove duplicates (Compare only 'e.flea' with 'flea.Eq'), returning a new
// list.
//
// Order of 'invs' is altered.
//    invs: investor list.
func RemoveDuplicates(invs []*T) []*T {
	var r []*T
	for _, inv := range invs {
		if !inv.IsIn(r) {
			r = append(r, inv)
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
//   source : Evaluated fleas to add.
//   target : Evaluated fleas target.
//   n      : Maximum length allowed of target.
func Complete(source, target []*T, n int) []*T {
	for _, inv := range source {
		if len(target) >= n {
			break
		}
		if !inv.IsIn(target) {
			target = append(target, inv)
		}
	}
	return target
}
