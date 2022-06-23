// Copyright 16-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Assets and operations number for tests
package assetsRs

import (
	"github.com/dedeme/ktlib/js"
)

type T struct {
	Assets float64
	Buys   int
	Sells  int
}

func New(assets float64, buys, sells int) *T {
	return &T{assets, buys, sells}
}

func ToJs(a *T) string {
	return js.Wa([]string{
		js.Wd(a.Assets),
		js.Wi(a.Buys),
		js.Wi(a.Sells),
	})
}
