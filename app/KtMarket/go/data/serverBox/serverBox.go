// Copyright 21-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Server selector.
package serverBox

import (
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/js"
)

type T struct {
	Servers []string
}

func New(svs []string) *T {
	return &T{svs}
}

func ToJs(b *T) string {
	return js.Wa(arr.Map(b.Servers, js.Ws))
}

func FromJs(j string) *T {
	return New(arr.Map(js.Ra(j), js.Rs))
}
