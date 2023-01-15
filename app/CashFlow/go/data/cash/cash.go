// Copyright 20-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package cash

import (
	"github.com/dedeme/ktlib/js"
)

type T []*Entry

func (d T) ToJs() string {
	var a []string
	for _, e := range d {
		a = append(a, e.ToJs())
	}
	return js.Wa(a)
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

func (e *Entry) ToJs() string {
	return js.Wa([]string{
		js.Ws(e.month),
		js.Ws(e.desc),
		js.Wb(e.isIncome),
		js.Wd(e.am),
	})
}
