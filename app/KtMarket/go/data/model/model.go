// Copyright 08-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Model data.
package model

import (
	"github.com/dedeme/KtMarket/data/reference"
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/js"
)

type T struct {
	// Short name.
	Id string
	// Long name.
	Name string
	// Documentation.
	Doc string
	// Parameter names.
	ParamNames []string
	// Maximum parameters value
	ParamMaxs []float64
	// Minimum parameters value
	ParamMins []float64
	// Number of decimal positios in parameters.
	//     0 - Integer
	//     4, 6 - Percentage.
	//     Other - Normal number with 'other' decimals positions.
	ParamDecs []int
	// Function to calculate operations.
	//    closes: Closes in matrix 'dates x cos' ordered from before to after.
	//    params: Values to calculate.
	//    refs  : Initial references. If ref.Ref < 0 the reference is discarded.
	//    action: Function called after calculate 'refs'
	//            Params:
	//              closes: Last closes. One for each company.
	//              refs  : Last references. One for each company.
	Calc func(
		closes [][]float64,
		params []float64,
		refs []*reference.T,
		action func(closes []float64, refs []*reference.T))
}

// NOTE: Parameter m.Calc is not serialized.
func ToJs(m *T) string {
	return js.Wa([]string{
		js.Ws(m.Id),
		js.Ws(m.Name),
		js.Ws(m.Doc),
		js.Wa(arr.Map(m.ParamNames, js.Ws)),
		js.Wa(arr.Map(m.ParamMaxs, js.Wd)),
		js.Wa(arr.Map(m.ParamMins, js.Wd)),
		js.Wa(arr.Map(m.ParamDecs, js.Wi)),
	})
}

// NOTE: 'j' was serialized with 'ToJs'.
//
// For finding the model the 'id' returned is searched in "List()' (list.go).
// If no model is found, panic is raised.
func FromJs(j string) *T {
	a := js.Ra(j)
	id := js.Rs(a[0])
	md, ok := arr.Find(List(), func(m *T) bool {
		return m.Id == id
	})
	if !ok {
		panic("Model '" + id + "' not found")
	}
	return md
}

// Serializes only 'm.Id' for using with tables.
func ToJsTb(m *T) string {
	return js.Ws(m.Id)
}

// Deserializes a model serialized with 'ToJsTb'
//
// For finding the model the 'id' returned is searched in "List()' (list.go).
// If no model is found, panic is raised.
func FromJsTb(j string) *T {
	id := js.Rs(j)
	md, ok := arr.Find(List(), func(m *T) bool {
		return m.Id == id
	})
	if !ok {
		panic("Model '" + id + "' not found")
	}
	return md
}
