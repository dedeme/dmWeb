// Copyright 07-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Ranking data.
package rank

import (
	"github.com/dedeme/QMarket/data/cts"
	"github.com/dedeme/QMarket/data/model"
	"github.com/dedeme/QMarket/data/qtable"
	"github.com/dedeme/golib/json"
)

type entryT struct {
	md    *model.T
	value float64
}

type T []*entryT

// Sorted ranking of ids from more to less
func New() T {
	return make([]*entryT, cts.RankingEntries)
}

// Adds a new id if is greater than the least of already included.
//    md   : Model identifier with or without evaluation.
//    value: Evaluation value
func (r T) Add(md *model.T, value float64) {
	i := cts.RankingEntries - 1
	for {
		e := r[i]
		if e != nil && e.value > value {
			break
		}
		i--
		if i < 0 {
			break
		}
	}

	i++
	for j := cts.RankingEntries - 1; j > i; j-- {
		r[j] = r[j-1]
	}
	if i < cts.RankingEntries {
		r[i] = &entryT{md, value}
	}
}

// Returns a slice with ordered evaluated models from better to worse of a
// Qlevel
//    qlevel: Quantum level of models.
//    opens : Opens table.
//    closes: Closes table.
//    getRs : Function to get a model result.
func (r T) MkRank(
	qlevel int, opens, closes *qtable.T, getRs func(*model.T) *model.RsT,
) []*model.T {
	var rt []*model.T
	for _, e := range r {
		rs := getRs(e.md)
		e.md.SetEvaluation(rs, opens, closes)
		rt = append(rt, e.md)
	}
	return rt
}

// Returns a slice with ordered evaluated models from better to worse in
// function of its average evaluation.
//
// The returns is a [cts.RankingEntries][cts.Qlevel]*model.T
//    opens : Opens table.
//    closes: Closes table.
//    getRs : Function to get models result, from Qlevel==0 (inclusive) to
//            Qlevel=cts.Qlevels (exclusive)
func (r T) MkAvgRank(
	opens, closes *qtable.T, getRs func(int) []*model.RsT,
) []AvgRowT {
	var rt []AvgRowT
	for _, e := range r {
		var row []*model.T
		mId := e.md.Id()
		rss := getRs(mId)
		for qlevel := 0; qlevel < cts.Qlevels; qlevel++ {
			md := model.New(qlevel, mId)
			md.SetEvaluation(rss[qlevel], opens, closes)
			row = append(row, md)
		}
		rt = append(rt, row)
	}
	return rt
}

// Table -----------------------------------------------------------------------

// Entry of a rankings table.
type TableEntryT struct {
	date      string
	modelRank []*model.T
}

func NewTableEntry(date string, modelRank []*model.T) *TableEntryT {
	return &TableEntryT{date, modelRank}
}

// Ranking date.
func (e *TableEntryT) Date() string {
	return e.date
}

// Ranking models
func (e *TableEntryT) ModelRank() []*model.T {
	return e.modelRank
}

func (e *TableEntryT) toJs() json.T {
	var mdJs []json.T
	for _, md := range e.modelRank {
		mdJs = append(mdJs, md.ToJs())
	}
	return json.Wa([]json.T{
		json.Ws(e.date),
		json.Wa(mdJs),
	})
}

func tableEntryFromJs(js json.T) *TableEntryT {
	a := js.Ra()
	var mds []*model.T
	for _, mdJs := range a[1].Ra() {
		mds = append(mds, model.FromJs(mdJs))
	}
	return &TableEntryT{
		a[0].Rs(),
		mds,
	}
}

// Table of model rankings of a Qlevel.
//
// Rankings are sorted from after to before.
type TableT []*TableEntryT

func NewTable() TableT {
	return make([]*TableEntryT, cts.RankingDays)
}

// Create a new table mixing 'rktbs'
func MixTables(rktbs []TableT) TableT {
	var r TableT
	for iday := 0; iday < cts.RankingDays; iday++ {
		newRk := New()
		var date string
		for irktb := 0; irktb < len(rktbs); irktb++ {
			rk := rktbs[irktb][iday]
			date = rk.date
			for _, md := range rk.modelRank {
				_, he := md.Hevaluation()
				newRk.Add(md, he.Value())
			}
		}
		var newTbRk []*model.T
		for _, e := range newRk {
			newTbRk = append(newTbRk, e.md)
		}
		r = append(r, NewTableEntry(date, newTbRk))
	}
	return r
}

// Modify rtb in place, adding or replacing a new ranking
//
//  'rk' will be added if its date > 'rtb.date'.
func (rtb TableT) Add(rk *TableEntryT) {
	if rtb[0] == nil {
		for i := 0; i < cts.RankingDays; i++ {
			rtb[i] = rk
		}
		return
	}
	if rk.date > rtb[0].date {
		for i := cts.RankingDays - 1; i > 0; i-- {
			rtb[i] = rtb[i-1]
		}
	}
	rtb[0] = rk
}

func (rtb TableT) ToJs() json.T {
	if rtb[0] == nil {
		return json.Wn()
	}
	var r []json.T
	for _, rk := range rtb {
		r = append(r, rk.toJs())
	}
	return json.Wa(r)
}

func TableFromJs(js json.T) TableT {
	if js.IsNull() {
		return NewTable()
	}
	var r []*TableEntryT
	for _, eJs := range js.Ra() {
		r = append(r, tableEntryFromJs(eJs))
	}
	return r
}

// AvgTable -------------------------------------------------------------------

type AvgRowT []*model.T

func (r AvgRowT) toJs() json.T {
	var rs []json.T
	for _, rr := range r {
		rs = append(rs, rr.ToJs())
	}
	return json.Wa(rs)
}

func avgRowFromJs(js json.T) AvgRowT {
	var rs AvgRowT
	for _, mjs := range js.Ra() {
		rs = append(rs, model.FromJs(mjs))
	}
	return rs
}

type AvgTableEntryT struct {
	date    string
	rowRank []AvgRowT
}

func NewAvgTableEntry(
	date string, rowRank []AvgRowT,
) *AvgTableEntryT {
	return &AvgTableEntryT{date, rowRank}
}

func (e *AvgTableEntryT) Date() string {
	return e.date
}

func (e *AvgTableEntryT) ModelRank() []AvgRowT {
	return e.rowRank
}

func (e *AvgTableEntryT) toJs() json.T {
	var rowJs []json.T
	for _, row := range e.rowRank {
		rowJs = append(rowJs, row.toJs())
	}
	return json.Wa([]json.T{
		json.Ws(e.date),
		json.Wa(rowJs),
	})
}

func avgTableEntryFromJs(js json.T) *AvgTableEntryT {
	a := js.Ra()
	var rows []AvgRowT
	for _, rowJs := range a[1].Ra() {
		rows = append(rows, avgRowFromJs(rowJs))
	}
	return &AvgTableEntryT{
		a[0].Rs(),
		rows,
	}
}

// Table of model rankings of every Qlevel for each model identifier.
//
// Rankings are sorted from after to before.
type AvgTableT []*AvgTableEntryT

func NewAvgTable() AvgTableT {
	return make([]*AvgTableEntryT, cts.RankingDays)
}

// Modify rtb in place, adding or replacing a new ranking
//
//  'rk' will be added if its date > 'rtb.date'.
func (rtb AvgTableT) Add(rk *AvgTableEntryT) {
	if rtb[0] == nil {
		for i := 0; i < cts.RankingDays; i++ {
			rtb[i] = rk
		}
		return
	}
	if rk.date > rtb[0].date {
		for i := cts.RankingDays - 1; i > 0; i-- {
			rtb[i] = rtb[i-1]
		}
	}
	rtb[0] = rk
}

func (rtb AvgTableT) ToJs() json.T {
	if rtb[0] == nil {
		return json.Wn()
	}
	var r []json.T
	for _, rk := range rtb {
		r = append(r, rk.toJs())
	}
	return json.Wa(r)
}

func AvgTableFromJs(js json.T) AvgTableT {
	if js.IsNull() {
		return NewAvgTable()
	}
	var r []*AvgTableEntryT
	for _, eJs := range js.Ra() {
		r = append(r, avgTableEntryFromJs(eJs))
	}
	return r
}
