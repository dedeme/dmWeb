// Copyright 03-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

//Configuration data.
package conf

import (
	"github.com/dedeme/KtMarket/data/activity"
	"github.com/dedeme/ktlib/js"
)

type T struct {
	Lang     string
	Activity *activity.T
}

// Constructor.
//   lang: 'en' or 'es'.
//   act : Last activity.
func New(lang string, act *activity.T) *T {
	return &T{lang, act}
}

func ToJs(cf *T) string {
	return js.Wa([]string{
		js.Ws(cf.Lang),
		activity.ToJs(cf.Activity),
	})
}

func FromJs(j string) *T {
	a := js.Ra(j)
	return New(
		js.Rs(a[0]),
		activity.FromJs(a[1]),
	)
}
