// Copyright 13-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Evaluate flea ranking entry.
package frank

import (
	"github.com/dedeme/MultiMarket/data/flea/eFlea"
	"github.com/dedeme/golib/json"
)

type T struct {
	date    string
	ranking []*eFlea.T
}

// Cretes a new 'frank'.
//    date   : Ranking date
//    ranking: Evaluated fleas list.
func New(date string, ranking []*eFlea.T) *T {
	return &T{date, ranking}
}

func (r *T) Date() string {
	return r.date
}

func (r *T) Ranking() []*eFlea.T {
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
	var rk []*eFlea.T
	for _, e := range a[1].Ra() {
		rk = append(rk, eFlea.FromJs(e))
	}
	return &T{
		a[0].Rs(),
		rk,
	}
}
