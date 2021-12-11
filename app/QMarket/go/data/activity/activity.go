// Copyright 16-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Activity data.
package activity

import (
	"github.com/dedeme/QMarket/data/calendar"
	"github.com/dedeme/QMarket/data/cts"
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

// Creates a new activity.T.
//    act: One of cts.ActXXX.
func New(d date.T, act string) *T {
	return &T{d, act}
}

// Creates a new activity.T with the current time.
//    act: One of cts.ActXXX.
func NewNow(act string) *T {
	return &T{date.Now(), act}
}

func Current(cal *calendar.T) *T {
	now := date.Now()
	if cal.IsMarketDay(now) {
		if cal.IsBeforeClosing(now) {
			if cal.IsOpen(now) {
				return &T{now, cts.ActActive}
			}
			if now.Hour() < cts.ActHistoricStart {
				return &T{now, cts.ActSleeping1}
			}
			return &T{now, cts.ActSleeping2}
		}
		return &T{now, cts.ActSleeping3}
	}
	return &T{now, cts.ActSleeping2}
}

// Creates a new activity.T updating 'act'
//    act: One of cts.ActXXX.
func (a *T) Update(cal *calendar.T) *T {
	crr := Current(cal)
	if crr.date.Eq(a.date) {
		return a
	}

	if crr.activity == cts.ActSleeping1 &&
		cal.PreviousMarketDay(crr.date) == crr.date.Add(-1) {
		return crr
	}
	return NewNow(cts.ActHistoric)
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
