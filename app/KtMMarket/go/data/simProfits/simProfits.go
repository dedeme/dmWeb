// Copyright 02-Jul-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Simulation profits data.
package simProfits

import (
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/js"
)

type T struct {
	// Total profits.
	Total float64
	// Cash profits.
	Cash float64
	// Refence (risk) profits.
	Ref float64
}

func New(total, cash, ref float64) *T {
	return &T{total, cash, ref}
}

func (pr *T) Sum(pr2 *T) *T {
	return New(
		pr.Total+pr2.Total,
		pr.Cash+pr2.Cash,
		pr.Ref+pr2.Ref,
	)
}

func (pr *T) Div(n int) *T {
	return New(
		pr.Total/float64(n),
		pr.Cash/float64(n),
		pr.Ref/float64(n),
	)
}

func ToJs(sp *T) string {
	return js.Wa([]string{
		js.Wd(sp.Total),
		js.Wd(sp.Cash),
		js.Wd(sp.Ref),
	})
}

func FromJs(j string) *T {
	a := js.Ra(j)
	return New(
		js.Rd(a[0]),
		js.Rd(a[1]),
		js.Rd(a[2]),
	)
}

// Table row
type RowT struct {
	// Params evaluated.
	Params []float64
	// Weeks weight to calculate historic values.
	Weeks int
	// Historic values.
	Hprofits *T
	// Last values.
	Profits *T
}

func NewRow(params []float64, weeks int, hprofits, profits *T) *RowT {
	return &RowT{params, weeks, hprofits, profits}
}

func RowToJs(r *RowT) string {
	return js.Wa([]string{
		js.Wa(arr.Map(r.Params, js.Wd)),
		js.Wi(r.Weeks),
		ToJs(r.Hprofits),
		ToJs(r.Profits),
	})
}

func RowFromJs(j string) *RowT {
	a := js.Ra(j)
	return NewRow(
		arr.Map(js.Ra(a[0]), js.Rd),
		js.Ri(a[1]),
		FromJs(a[2]),
		FromJs(a[3]),
	)
}

type TbT struct {
	// Date of data in format YYYYMMDD.
	Date string
	// Rows data.
	Rows []*RowT
}

func NewTb(date string, rows []*RowT) *TbT {
	return &TbT{date, rows}
}

func TbToJs(t *TbT) string {
	return js.Wa([]string{
		js.Ws(t.Date),
		js.Wa(arr.Map(t.Rows, RowToJs)),
	})
}

func TbFromJs(j string) *TbT {
	a := js.Ra(j)
	return NewTb(
		js.Rs(a[0]),
		arr.Map(js.Ra(a[1]), RowFromJs),
	)
}
