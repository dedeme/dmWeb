// Copyright 12-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Data for managing ranking of jump results
package jumpResult

import (
	"github.com/dedeme/MultiMarket/data/flea/investor"
	"github.com/dedeme/golib/json"
)

type T struct {
	param     float64
	eval      float64
	sales     float64
	investors []*investor.T
}

func New(param, eval, sales float64, investors []*investor.T) *T {
	return &T{param, eval, sales, investors}
}

func (r *T) ToJs() json.T {
	var invsJs []json.T
	for _, e := range r.investors {
		invsJs = append(invsJs, e.ToJs())
	}
	return json.Wa([]json.T{
		json.Wd(r.param),
		json.Wd(r.eval),
		json.Wd(r.sales),
		json.Wa(invsJs),
	})
}

func (r *T) ToJsClient() json.T {
	var invsJs []json.T
	for _, e := range r.investors {
		invsJs = append(invsJs, e.ToJsClient())
	}
	return json.Wa([]json.T{
		json.Wd(r.param),
		json.Wd(r.eval),
		json.Wd(r.sales),
		json.Wa(invsJs),
	})
}

func FromJs(js json.T) *T {
	a := js.Ra()
	var invs []*investor.T
	for _, e := range a[3].Ra() {
		inv, _ := investor.FromJs(e)
		invs = append(invs, inv)
	}
	return &T{
		a[0].Rd(),
		a[1].Rd(),
		a[2].Rd(),
		invs,
	}
}
