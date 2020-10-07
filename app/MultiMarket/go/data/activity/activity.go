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
	date date.T
	// One of cts.ActXXX.
	activity string
}

// Starting date.
func (a *T) Date() date.T {
	return a.date
}

// One of cts.ActXXX.
func (a *T) Activity() string {
	return a.activity
}

// Creates a new activity.t with the current time.
//    act: One of cts.ActXXX.
func New(act string) *T {
	return &T{date.Now(), act}
}

// Creates a new activity.t for debug.
//    act: One of cts.ActXXX.
func New2(d date.T, act string) *T {
	return &T{d, act}
}

func (ac *T) ToJs() json.T {
	return json.Wa([]json.T{
		json.Wl(time.Time(ac.date).Unix()),
		json.Ws(ac.activity),
	})
}

func FromJs(js json.T) *T {
	a := js.Ra()
	return &T{
		date.T(time.Unix(a[0].Rl(), 0)),
		a[1].Rs(),
	}
}
