// Copyright 05-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Market day data.
package marketDay

import (
	"github.com/dedeme/golib/json"
)

type T struct {
  date string
	hopen  int
	hclose int
	mopen  int
	mclose int
}

func (tt *T) Date() string {
	return tt.date
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
    json.Ws(tt.date),
		json.Wi(tt.hopen), json.Wi(tt.hclose),
		json.Wi(tt.mopen), json.Wi(tt.mclose),
	})
}

func FromJs(js json.T) *T {
	a := js.Ra()
	return &T{a[0].Rs(), a[1].Ri(), a[2].Ri(), a[3].Ri(), a[4].Ri()}
}
