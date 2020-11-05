// Copyright 11-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Flea data.
package flea

import (
	"github.com/dedeme/MultiMarket/data/cts"
	"github.com/dedeme/MultiMarket/data/flea/fmodel"
	"github.com/dedeme/MultiMarket/global/fn"
	"github.com/dedeme/golib/date"
	"github.com/dedeme/golib/json"
	"math/rand"
	"strconv"
)

type T struct {
	date   string
	cycle  int
	id     int
	params []float64
}

// Random constructor
//    md: Flea model.
//    date: Date of creation.
//    cycle: Cycle of creation.
//    id: Identifier inside date-cycle.
func New(md *fmodel.T, date string, cycle, id int) *T {
	var params []float64
	mins := md.ParMins()
	maxs := md.ParMaxs()
	for i, e := range md.ParDecs() {
		mn := mins[i]
		params = append(params, fn.Fix(mn+(maxs[i]-mn)*rand.Float64(), e))
	}

	return &T{date, cycle, id, params}
}

// Date of creation.
func (f *T) Date() string {
	return f.date
}

// Cycle of creation.
func (f *T) Cycle() int {
	return f.cycle
}

// Identifier inside date-cycle.
func (f *T) Id() int {
	return f.id
}

// Model parameters.
func (f *T) Params() []float64 {
	return f.params
}

// Returns date-cycle-id
func (f *T) Name() string {
	return f.date + "-" + strconv.Itoa(f.cycle) + "-" + strconv.Itoa(f.id)
}

// Two fleas are equals if they have equals 'params'.
func (f *T) Eq(f2 *T) (ok bool) {
	p1 := f.params
	p2 := f2.params
	if len(p1) == len(p2) {
		for i, e := range p1 {
			df := e - p2[i]
			if df > 0.0000001 || df < -0.0000001 {
				return
			}
		}
		ok = true
	}
	return
}

// Evaluate a flea.
//
// Ratios of assets and profitsAvg are 0 -> -100%, 1 -> 0%, 2 -> 100%,
// 3 -> 200%...
//    f         : Flea
//    assets    : Ratio of assets
//    profitsAvg: Ratio of profits average.
//    profitsSd : Ratio of profits standard deviation (between 0 and 1).
func (f *T) Evaluate(assets, profitsAvg, profitsSd float64) float64 {
	age := float64(date.Now().Df(date.FromString(f.date)))
	if age >= cts.HistoricQuotes {
		age = float64(1)
	} else {
		age = age / float64(cts.HistoricQuotes)
	}
	pond := 1.0
  if assets < 1.1 {
    pond = 0.5
  }
  if profitsAvg < 1 {
    pond = pond * 0.5
  }
  if profitsSd > 0.25 {
    pond = pond * 0.5
  }
	return assets*pond*float64(cts.AssetsRatio) +
		profitsAvg*pond*float64(cts.ProfitsAvgRatio) +
    (1 - profitsSd)*pond*float64(cts.ProfitsSdRatio) +
		age*pond*float64(cts.AgeRatio)
}

// Returns a new muted flea.
//   md   : Model
//   date : Date of creation.
//   cycle: Cycle of creation
//   id   : Identifier inside of date-cycle.
func (f *T) Mutate(md *fmodel.T, date string, cycle, id int) *T {
	var params []float64
	mins := md.ParMins()
	maxs := md.ParMaxs()
	decs := md.ParDecs()
	for i, e := range f.params {
		rnd := rand.Float64()
		mul := cts.MutationMultiplier * (rnd + rnd - 1)
		var df float64
		if mul > 0 {
			df = maxs[i] - e
		} else {
			df = e - mins[i]
		}
		params = append(params, fn.Fix(e+mul*df, decs[i]))
	}
	return &T{date, cycle, id, params}
}

func (f *T) ToJs() json.T {
	var params []json.T
	for _, e := range f.params {
		params = append(params, json.Wd(e))
	}
	return json.Wa([]json.T{
		json.Ws(f.date),
		json.Wi(f.cycle),
		json.Wi(f.id),
		json.Wa(params),
	})
}

func FromJs(js json.T) *T {
	a := js.Ra()
	var params []float64
	for _, e := range a[3].Ra() {
		params = append(params, e.Rd())
	}
	return &T{
		a[0].Rs(),
		a[1].Ri(),
		a[2].Ri(),
		params,
	}
}
