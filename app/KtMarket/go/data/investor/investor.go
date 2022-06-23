// Copyright 15-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package investor

import (
	"github.com/dedeme/KtMarket/data/model"
	"github.com/dedeme/KtMarket/data/strategy"
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/js"
)

type T struct {
	// Strategy by default.
	Base *strategy.T
	// Strategy for every company.
	Nicks map[string]*strategy.T
}

func New(base *strategy.T, nks map[string]*strategy.T) *T {
	return &T{base, nks}
}

func ToJs(inv *T) string {
	nicksJs := map[string]string{}
	for k, v := range inv.Nicks {
		nicksJs[k] = strategy.ToJs(v)
	}
	return js.Wa([]string{
		strategy.ToJs(inv.Base),
		js.Wo(nicksJs),
	})
}

// Serialization only for using with tables in connection with 'FromJsTb'
func ToJsTb(inv *T) string {
	nicksJs := map[string]string{}
	for k, v := range inv.Nicks {
		nicksJs[k] = strategy.ToJsTb(v)
	}
	return js.Wa([]string{
		strategy.ToJsTb(inv.Base),
		js.Wo(nicksJs),
	})
}

// Deserializes a model serialized with 'ToJsTb'
func FromJsTb(j string) *T {
	a := js.Ra(j)
	nks := map[string]*strategy.T{}
	for k, v := range js.Ro(a[1]) {
		nks[k] = strategy.FromJsTb(v)
	}
	return New(
		strategy.FromJsTb(a[0]),
		nks,
	)
}

// TbT -------------------------------------------------------------------------

type TbT struct {
	Investors []*T
}

func NewTb(invs []*T) *TbT {
	return &TbT{invs}
}

func DefaultTable() *TbT {
	md := model.List()[0]
	var params []float64
	for i := range md.ParamNames {
		params = append(params, (md.ParamMaxs[i]+md.ParamMins[i])/2.0)
	}
	st := strategy.New(md, params)

	return &TbT{[]*T{New(st, map[string]*strategy.T{})}}
}

func TbToJs(t *TbT) string {
	return js.Wa(arr.Map(t.Investors, ToJsTb))
}

func TbFromJs(j string) *TbT {
	return &TbT{arr.Map(js.Ra(j), FromJsTb)}
}
