// Copyright 13-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Investors ranking entry.
package irank

import (
	"github.com/dedeme/MultiMarket/data/flea/investor"
	"github.com/dedeme/golib/json"
)

type T struct {
	date    string
	ranking []*investor.T
}

// Cretes a new 'irank'.
//    date   : Ranking date
//    ranking: Investors list.
func New(date string, ranking []*investor.T) *T {
	return &T{date, ranking}
}

func (r *T) Date() string {
	return r.date
}

func (r *T) Ranking() []*investor.T {
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

func (r *T) ToJsClient() json.T {
	var rk []json.T
	for _, e := range r.ranking {
		rk = append(rk, e.ToJsClient())
	}
	return json.Wa([]json.T{
		json.Ws(r.date),
		json.Wa(rk),
	})
}

func FromJs(js json.T) *T {
	a := js.Ra()
	return &T{
		a[0].Rs(),
		investor.FromJsList(a[1].Ra()),
	}
}
