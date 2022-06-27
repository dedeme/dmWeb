// Copyright 18-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Investor operation
package invOperation

import (
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/js"
)

type T struct {
	// NOTE: If stocks > 0 is a sell operation.
	//       If stocks == 0 is a normal buy operation.
	//       if stocks < 0 is a rebuy
	Stocks  int
	Investor int
	Nick    string
}

// NOTE: If stocks > 0 is a sell operation.
//       If stocks == 0 is a normal buy operation.
//       if stocks < 0 is a rebuy
func New(stocks, inv int, nickName string) *T {
	return &T{stocks, inv, nickName}
}

func ToJs(o *T) string {
	return js.Wa([]string{
		js.Wi(o.Stocks),
		js.Wi(o.Investor),
		js.Ws(o.Nick),
	})
}

func FromJs(j string) *T {
	a := js.Ra(j)
	return New(
		js.Ri(a[0]),
		js.Ri(a[1]),
		js.Rs(a[2]),
	)
}

// TABLE

type TbT struct {
	Operations []*T
}

func NewTb(ops []*T) *TbT {
	return &TbT{ops}
}

func TbToJs(t *TbT) string {
	return js.Wa(arr.Map(t.Operations, ToJs))
}

func TbFromJs(j string) *TbT {
	return NewTb(arr.Map(js.Ra(j), FromJs))
}
