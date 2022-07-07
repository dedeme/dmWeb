// Copyright 18-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Base reference of a company
package refBase

import (
	"github.com/dedeme/KtMarket/cts"
	"github.com/dedeme/KtMarket/data/nick"
	"github.com/dedeme/KtMarket/data/strategy"
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/js"
)

type T struct {
	Nick     *nick.T
	Strategy *strategy.T
	Date     string
	Ref      float64
}

func New(nk *nick.T, st *strategy.T, date string, ref float64) *T {
	return &T{nk, st, date, ref}
}

// Calculate refBase of 'dates'-'closes'
func MkRefBase(
	nk *nick.T, st *strategy.T, dates []string, closes []float64,
) *T {
  n := len(closes)-cts.ReferenceQuotes-1
	cs := arr.Take(closes, n)
	lastRef := strategy.LastRef(st, cs, -1)
	return New(nk, st, dates[n-1], lastRef)
}

// Recalculate 'rf' from 'dates'-'closes'
func UpdateRefBase(rf *T, dates []string, closes []float64) *T {
	newDate := dates[len(closes)-cts.ReferenceQuotes-1]

	ds := arr.DropWhile(dates, func(d string) bool {
		return d <= rf.Date
	})
	cs := arr.Drop(closes, len(closes)-len(ds))

	ds = arr.TakeWhile(ds, func(d string) bool {
		return d <= newDate
	})
	cs = arr.Take(cs, len(ds))

	ref := rf.Ref
	if len(cs) > 0 {
		ref = strategy.LastRef(rf.Strategy, cs, ref)
	}

	return New(rf.Nick, rf.Strategy, newDate, ref)
}

func ToJs(r *T) string {
	return js.Wa([]string{
		nick.ToJs(r.Nick),
		strategy.ToJsTb(r.Strategy),
		js.Ws(r.Date),
		js.Wd(r.Ref),
	})
}

func FromJs(j string) *T {
	a := js.Ra(j)
	return New(
		nick.FromJs(a[0]),
		strategy.FromJsTb(a[1]),
		js.Rs(a[2]),
		js.Rd(a[3]),
	)
}

// TABLE

type TbT struct {
	Refs []*T
}

func NewTb(refs []*T) *TbT {
	return &TbT{refs}
}

func (t *TbT) Get(nk *nick.T, st *strategy.T) (ref *T, ok bool) {
	return arr.Find(t.Refs, func(rf *T) bool {
		return nk.Id == rf.Nick.Id && strategy.Eq(st, rf.Strategy)
	})
}

func TbToJs(t *TbT) string {
	return js.Wa(arr.Map(t.Refs, ToJs))
}

func TbFromJs(j string) *TbT {
	return NewTb(arr.Map(js.Ra(j), FromJs))
}
