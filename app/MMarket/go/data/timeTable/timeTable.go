// Copyright 05-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Time table data.
package timeTable

import (
	"github.com/dedeme/golib/json"
)

type T struct {
	hopen  int
	mopen  int
	hclose int
	mclose int
}

func New() *T {
	return &T{0, 0, 23, 55}
}

func (tt *T) Hopen() int {
	return tt.hopen
}

func (tt *T) Hclose() int {
	return tt.hclose
}

func (tt *T) Mopen() int {
	return tt.mopen
}

func (tt *T) Mclose() int {
	return tt.mclose
}

func (tt *T) ToJs() json.T {
	return json.Wa([]json.T{
		json.Wi(tt.hopen), json.Wi(tt.mopen),
    json.Wi(tt.hclose), json.Wi(tt.mclose),
	})
}

func FromJs(js json.T) *T {
	a := js.Ra()
	return &T{a[0].Ri(), a[1].Ri(), a[2].Ri(), a[3].Ri()}
}
