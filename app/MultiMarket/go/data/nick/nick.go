// Copyright 20-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Nick data.
package nick

import (
	"github.com/dedeme/golib/json"
)

type T struct {
	// Identifier.
	Id int
	// Name (TEF, ACS...).
	Name string
	// If is selected to operate.
	IsSel bool
}

type QvalueT struct {
	// Nick id.
	Nick int
	// A quote value: open, close, max, min, vol.
	Value float64
}

// Constructor.
//    id  : Identifier.
//    name: Name (TEF, ACS...).
func New(id int, name string) *T {
	return &T{id, name, false}
}

func (n *T) ToJs() json.T {
	return json.Wa([]json.T{
		json.Wi(n.Id),
		json.Ws(n.Name),
		json.Wb(n.IsSel),
	})
}

func FromJs(js json.T) *T {
	a := js.Ra()
	return &T{
		a[0].Ri(),
		a[1].Rs(),
		a[2].Rb(),
	}
}

// Constructor
//    nk   : Nick id.
//    value: A quote value: open, close, max, min, vol.
func NewQvalue(nk int, value float64) *QvalueT {
	return &QvalueT{nk, value}
}

func (qv *QvalueT) QvalueToJs() json.T {
	return json.Wa([]json.T{
		json.Wi(qv.Nick),
		json.Wd(qv.Value),
	})
}

func QvalueFromJs(js json.T) *QvalueT {
	a := js.Ra()
	return &QvalueT{
		a[0].Ri(),
		a[1].Rd(),
	}
}
