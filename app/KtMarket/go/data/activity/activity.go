// Copyright 03-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Activity data.
package activity

import (
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/time"
)

type T struct {
	Time     int64
	Activity string
}

// Constructor.
//   tm : Record time.
//   act: One of cts.ActXXX
func New(tm int64, act string) *T {
	return &T{tm, act}
}

// Constructor with current time.
//   act : One of cts.ActXXX
func NewNow(act string) *T {
	return New(time.Now(), act)
}

func ToJs(act *T) string {
	return js.Wa([]string{
		js.Wl(act.Time),
		js.Ws(act.Activity),
	})
}

func FromJs(j string) *T {
	a := js.Ra(j)
	return New(
		js.Rl(a[0]),
		js.Rs(a[1]),
	)
}
