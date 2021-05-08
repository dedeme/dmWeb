// Copyright 13-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Timetable data.
package timetable

import (
	"github.com/dedeme/golib/json"
)

type T struct {
	hopen  int
	mopen  int
	hclose int
	mclose int
}

// Constructor
func New() *T {
	return &T{0, 0, 23, 55}
}

// Constructor for debug
func New2(hopen, mopen, hclose, mclose int) *T {
	return &T{hopen, mopen, hclose, mclose}
}

// Open hour.
func (tt *T) Hopen() int {
	return tt.hopen
}

// Open minute.
func (tt *T) Mopen() int {
	return tt.mopen
}

// Close hour.
func (tt *T) Hclose() int {
	return tt.hclose
}

// Close minute.
func (tt *T) Mclose() int {
	return tt.mclose
}

func (tt *T) ToJs() json.T {
	return json.Wa([]json.T{
		json.Wi(tt.hopen),
		json.Wi(tt.mopen),
		json.Wi(tt.hclose),
		json.Wi(tt.mclose),
	})
}

func FromJs(js json.T) *T {
	a := js.Ra()
	return &T{
		a[0].Ri(),
		a[1].Ri(),
		a[2].Ri(),
		a[3].Ri(),
	}
}
