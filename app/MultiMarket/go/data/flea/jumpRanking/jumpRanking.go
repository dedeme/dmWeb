// Copyright 12-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Data for managing ranking of jump results
package jumpRanking

import (
	"github.com/dedeme/MultiMarket/data/cts"
	"github.com/dedeme/MultiMarket/data/flea/jumpResult"
	"github.com/dedeme/golib/json"
)

type T struct {
	date    string
	ranking []*jumpResult.T
}

func New(date string, ranking []*jumpResult.T) *T {
	return &T{date, ranking}
}

func (r *T) ToJs() json.T {
	var rkJs []json.T
	for _, e := range r.ranking {
		rkJs = append(rkJs, e.ToJs())
	}
	return json.Wa([]json.T{
		json.Ws(r.date),
		json.Wa(rkJs),
	})
}

func (r *T) ToJsClient() json.T {
	var rkJs []json.T
	for _, e := range r.ranking {
		rkJs = append(rkJs, e.ToJsClient())
	}
	return json.Wa([]json.T{
		json.Ws(r.date),
		json.Wa(rkJs),
	})
}

func FromJs(js json.T) *T {
	a := js.Ra()
	var rk []*jumpResult.T
	for _, e := range a[1].Ra() {
		rk = append(rk, jumpResult.FromJs(e))
	}
	return &T{
		a[0].Rs(),
		rk,
	}
}

// Add rank a ranks and returns the new ranks.
func Add(ranks []*T, rank *T) []*T {
	for len(ranks) < cts.RankingDays {
		ranks = append(ranks, rank)
	}

	if rank.date == ranks[0].date {
		ranks[0] = rank
	} else if rank.date > ranks[0].date {
		ranks = append([]*T{rank}, ranks...)[:cts.RankingDays]
	}

	return ranks
}
