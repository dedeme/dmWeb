// Copyright 18-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Reference and positión
package reference

import (
	"github.com/dedeme/ktlib/js"
)

type T struct {
	Ref         float64
	InPortfolio bool
}

func New(ref float64, inPortfolio bool) *T {
	return &T{ref, inPortfolio}
}

/// Creates an empty reference with 'Ref' == -1.
func NewEmpty() *T {
	return &T{-1, true}
}

func ToJs(r *T) string {
	return js.Wa([]string{
		js.Wd(r.Ref),
		js.Wb(r.InPortfolio),
	})
}

func FromJs(j string) *T {
	a := js.Ra(j)
	return New(
		js.Rd(a[0]),
		js.Rb(a[1]),
	)
}
