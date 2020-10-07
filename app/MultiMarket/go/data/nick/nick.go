// Copyright 20-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Nick data.
package nick

import (
	"github.com/dedeme/golib/json"
)

type T struct {
	// Identifier.
	id int
	// Name (TEF, ACS...).
	name string
	// If is selected to operate.
	isSel bool
}

// Constructor.
//    id  : Identifier.
//    name: Name (TEF, ACS...).
func New(id int, name string) *T {
	return &T{id, name, false}
}

// Identifier.
func (n *T) Id() int {
	return n.id
}

// Name (TEF, ACS...).
func (n *T) Name() string {
	return n.name
}

// Set name (TEF, ACS...).
func (n *T) SetName(s string) {
	n.name = s
}

// If is selected to operate.
func (n *T) IsSel() bool {
	return n.isSel
}

// Set if is selected to operate.
func (n *T) SetSel(v bool) {
	n.isSel = v
}

func (n *T) ToJs() json.T {
	return json.Wa([]json.T{
		json.Wi(n.id),
		json.Ws(n.name),
		json.Wb(n.isSel),
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

type QvalueT struct {
	// Nick id.
	Nick int
	// A quote value: open, close, max, min, vol.
	Value float64
}

// Constructor
//    nk   : Nick id.
//    value: A quote value: open, close, max, min, vol.
func NewQvalue(nk int, value float64) *QvalueT {
	return &QvalueT{nk, value}
}

func (qv *QvalueT) ToJs() json.T {
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
