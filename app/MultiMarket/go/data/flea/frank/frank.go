// Copyright 13-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Evaluate flea ranking entry.
package frank

import (
	"github.com/dedeme/MultiMarket/data/flea/eval"
	"github.com/dedeme/golib/json"
)

type T struct {
	date    string
	ranking []*eval.T
}

// Cretes a new 'frank'.
//    date   : Ranking date
//    ranking: Evaluated fleas list.
func New(date string, ranking []*eval.T) *T {
	return &T{date, ranking}
}

func (r *T) Date() string {
	return r.date
}

func (r *T) Ranking() []*eval.T {
	return r.ranking
}

func (r *T) ToJs() json.T {
	var rk []json.T
	for _, e := range r.ranking {
		rk = append(rk, e.ToJs())
	}
	return json.Wa([]json.T{
		json.Ws(r.date),
		json.Wa(rk),
	})
}

func FromJs(js json.T) *T {
	a := js.Ra()
	var rk []*eval.T
	for _, e := range a[1].Ra() {
		rk = append(rk, eval.FromJs(e))
	}
	return &T{
		a[0].Rs(),
		rk,
	}
}
