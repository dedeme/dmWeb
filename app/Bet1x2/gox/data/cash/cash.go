// Copyright 20-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package cash

import (
	"github.com/dedeme/golib/json"
)

type T []*Entry

func (d T) ToJs() json.T {
	var a []json.T
	for _, e := range d {
		a = append(a, e.ToJs())
	}
	return json.Wa(a)
}

type Entry struct {
	month    string
	desc     string
	isIncome bool
	am       float64
}

func NewEntry(month, desc string, isIncome bool, am float64) *Entry {
	return &Entry{month, desc, isIncome, am}
}

func (e *Entry) Amount() float64 {
	return e.am
}

func (e *Entry) ToJs() json.T {
	return json.Wa([]json.T{
		json.Ws(e.month),
		json.Ws(e.desc),
		json.Wb(e.isIncome),
		json.Wd(e.am),
	})
}
