// Copyright 16-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Activity data.
package activity

import (
	"github.com/dedeme/golib/date"
	"github.com/dedeme/golib/json"
	"time"
)

type T struct {
	// Record date.
	Date date.T
	// One of cts.ActXXX.
	Activity string
}

// Creates a new activity.t with the current time.
//    act: One of cts.ActXXX.
func New(act string) *T {
	return &T{date.Now(), act}
}

func (ac *T) ToJs() json.T {
	return json.Wa([]json.T{
		json.Wl(time.Time(ac.Date).Unix()),
		json.Ws(ac.Activity),
	})
}

func FromJs(js json.T) *T {
	a := js.Ra()
	return &T{
		date.T(time.Unix(a[0].Rl(), 0)),
		a[1].Rs(),
	}
}
