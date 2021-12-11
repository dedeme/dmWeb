// Copyright 02-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Investors data.
package investors

import (
	"github.com/dedeme/QMarket/data/cts"
	"github.com/dedeme/QMarket/data/nick"
	"github.com/dedeme/golib/json"
)

// Map [nickName][Qlevels]pameterIdentifier.
//
// Only selected nicks are used.
type T map[string][]int

// Regularizes investors in place. Returns 'true' if invs was modified.
//    base: Base parameter identifier.
//    nicks: Selected nicks list.
//    isToSell: Return 'true' if the current position of a company is for sale.
//      nickName: Company nick name.
//      qlevel  : Model level.
//      id      : Model parameter identifier.
func (params T) Regularize(
	base int, nicks []*nick.T, isToSell func(string, int, int) bool,
) (modified bool) {
	for _, nk := range nicks {
		nkName := nk.Name()
		if mdIds, ok := params[nkName]; ok {
			for i := 0; i < cts.Qlevels; i++ {
				p := mdIds[i]
				if p != base {
					baseIsToSell := isToSell(nkName, i, base)
					paramIsToSell := isToSell(nkName, i, p)
					if baseIsToSell == paramIsToSell {
						mdIds[i] = base
						modified = true
					}
				}
			}
		} else {
			var mdIds []int
			for i := 0; i < cts.Qlevels; i++ {
				mdIds = append(mdIds, base)
				modified = true
			}
			params[nkName] = mdIds
		}
	}

	var toDeletes []string
	for k := range params {
		found := false
		for _, nk := range nicks {
			if nk.Name() == k {
				found = true
				break
			}
		}
		if !found {
			toDeletes = append(toDeletes, k)
		}
	}
	for _, k := range toDeletes {
		delete(params, k)
		modified = true
	}
	return
}

func (invs T) toJs() json.T {
	entriesJs := map[string]json.T{}
	for k, v := range invs {
		var iparams []json.T
		for i, ip := range v {
			if i < cts.Qlevels {
				iparams = append(iparams, json.Wi(ip))
			}
		}
		if len(iparams) < cts.Qlevels {
			iparams = append(iparams, json.Wi(cts.ParamIdMedium))
		}
		entriesJs[k] = json.Wa(iparams)
	}
	return json.Wo(entriesJs)
}

func fromJs(js json.T) T {
	invs := T{}
	for k, v := range js.Ro() {
		var iparams []int
		for i, ipJs := range v.Ra() {
			if i < cts.Qlevels {
				iparams = append(iparams, ipJs.Ri())
			}
		}
		if len(iparams) < cts.Qlevels {
			iparams = append(iparams, cts.ParamIdMedium)
		}
		invs[k] = iparams
	}
	return invs
}

// TableT ----------------------------------------------------------------------

type TableT struct {
	// Paramater identifier base
	Base int
	// map[nickName][Qlevels]pameterIdentifier
	Params T
}

func NewTable() *TableT {
	return &TableT{cts.ParamIdMedium, T{}}
}

func (t *TableT) ToJs() json.T {
	return json.Wa([]json.T{
		json.Wi(t.Base),
		t.Params.toJs(),
	})
}

func TableFromJs(js json.T) *TableT {
	a := js.Ra()
	return &TableT{
		a[0].Ri(),
		fromJs(a[1]),
	}
}
