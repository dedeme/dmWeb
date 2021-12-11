// Copyright 16-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Market day data.
package marketDay

import (
	"github.com/dedeme/golib/json"
)

type T struct {
	date   string
	hopen  int
	mopen  int
	hclose int
	mclose int
}

// Market day
func (tt *T) Date() string {
	return tt.date
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
		json.Ws(tt.date),
		json.Wi(tt.hopen),
		json.Wi(tt.mopen),
		json.Wi(tt.hclose),
		json.Wi(tt.mclose),
	})
}

func FromJs(js json.T) *T {
	a := js.Ra()
	return &T{
		a[0].Rs(),
		a[1].Ri(),
		a[2].Ri(),
		a[3].Ri(),
		a[4].Ri(),
	}
}
