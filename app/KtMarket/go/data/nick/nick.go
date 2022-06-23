// Copyright 04-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Nick and Nicks table data
package nick

import (
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/js"
)

// NICK

// Nicks are created automatically when are added to a list with 'nick.Add'.
type T struct {
	// Identifier
	Id int
	// Name (TEF, IBE, ADX...)
	Name string
	// True if nick is selected to operate
	IsSel bool
}

func ToJs(nk *T) string {
	return js.Wa([]string{
		js.Wi(nk.Id),
		js.Ws(nk.Name),
		js.Wb(nk.IsSel),
	})
}

func FromJs(j string) *T {
	a := js.Ra(j)
	return &T{
		js.Ri(a[0]),
		js.Rs(a[1]),
		js.Rb(a[2]),
	}
}

// PAIR NICK-FLOAT

// Container nickId - value
type IdValT struct {
	// Nick id.
	Nick int
	// A quote value: open, close, max, min, vol or other float64 value.
	Value float64
}

// Constructor
//    nk   : Nick id.
//    value: A quote value: open, close, max, min, vol, or other float64 value.
func NewIdVal(nk int, value float64) *IdValT {
	return &IdValT{nk, value}
}

func IdValToJs(qv *IdValT) string {
	return js.Wa([]string{
		js.Wi(qv.Nick),
		js.Wd(qv.Value),
	})
}

func IdValFromJs(j string) *IdValT {
	a := js.Ra(j)
	return &IdValT{
		js.Ri(a[0]),
		js.Rd(a[1]),
	}
}

// Container NickName - String
type NameStrT struct {
	// Nick name
	Nick  string
	Value string
}

func NewNameStr(nkName string, value string) *NameStrT {
	return &NameStrT{nkName, value}
}

func NameStrToJs(ns *NameStrT) string {
	return js.Wa([]string{
		js.Ws(ns.Nick),
		js.Ws(ns.Value),
	})
}

func NameStrFromJs(j string) *NameStrT {
	a := js.Ra(j)
	return &NameStrT{
		js.Rs(a[0]),
		js.Rs(a[1]),
	}
}

// NIKS TABLE

// Nicks table
type TbT struct {
	NextId int
	Model  int
	List   []*T
}

// Creates a table of nicks.
//   nextId: Next nick identifier.
//   model : Id of nick model for testing quotes or -1 if it is not set.
//   Lst   : List of nicks.
func NewTb(nextId, model int, lst []*T) *TbT {
	return &TbT{nextId, model, lst}
}

// Returns the selected nicks.
func (t *TbT) SelectedNicks() []*T {
	return arr.Filter(t.List, func(n *T) bool {
		return n.IsSel
	})
}

// Returns a nick from its 'id' or "ok==false"
func (t *TbT) NickFromId(id int) (nk *T, ok bool) {
	return arr.Find(t.List, func(n *T) bool {
		return n.Id == id
	})
}

// Add a new nick if there is no one with the same name, otherwise returns
// 'ok==false'
//    t : Table to add a new nick.
//    nickName: Nick name (TEF, BBVA, ...)
//    RETURN:
//      newT: A new table after add 'nk'.
//      nk  : The new nick added.
//      ok  : 'false' if nickName is duplicated.
func (t *TbT) Add(nickName string) (newT *TbT, nk *T, ok bool) {
	id := t.NextId
	nk = &T{id, nickName, false}
	for _, n := range t.List {
		if n.Name == nickName {
			return
		}
	}
	newT = &TbT{id + 1, t.Model, append(t.List, nk)}
	ok = true
	return
}

// Returns a new table with 'nickId' removed.
func (t *TbT) Del(nickId int) *TbT {
	nks := arr.Filter(t.List, func(n *T) bool {
		return n.Id != nickId
	})

	md := t.Model
	if t.Model == nickId {
		if len(nks) > 0 {
			md = nks[0].Id
		} else {
			md = -1
		}
	}

	return NewTb(t.NextId, md, nks)
}

// Modify name of 'nickId'.
//    nickId: Identifier of nick to change.
//    name: new name.
//    Return:
//      newT   : A new table after modification.
//      oldName: The changed name. If the new name is equals to the old name,
//               the function do nothing.
//      ok     : Return false if 'name' is duplicated or 'nickId' is missing.
func (t *TbT) Modify(nickId int, name string) (
	newT *TbT, oldName string, ok bool,
) {
	nk, idOk := t.NickFromId(nickId)
	if !idOk {
		return
	}

	oldName = nk.Name
	if oldName == name {
		newT = t
		ok = true
		return
	}

	if arr.Any(t.List, func(n *T) bool {
		return n.Name == name
	}) {
		return
	}

	nks := arr.Map(t.List, func(n *T) *T {
		if n.Id == nickId {
			return &T{nickId, name, n.IsSel}
		}
		return n
	})

	newT = NewTb(t.NextId, t.Model, nks)
	ok = true
	return
}

// Returns a new table setting the value 'isSel' of a nick if it exists
func (t *TbT) SetSel(nickId int, isSel bool) *TbT {
	nks := arr.Map(t.List, func(n *T) *T {
		if n.Id == nickId {
			return &T{nickId, n.Name, isSel}
		}
		return n
	})
	return NewTb(t.NextId, t.Model, nks)
}

// Set 'nickId' as nick model if it exists
func (t *TbT) SetModel(nickId int) *TbT {
	_, ok := t.NickFromId(nickId)
	if !ok {
		return t
	}
	return &TbT{t.NextId, nickId, t.List}
}

func TbToJs(t *TbT) string {
	return js.Wa([]string{
		js.Wi(t.NextId),
		js.Wi(t.Model),
		js.Wa(arr.Map(t.List, ToJs)),
	})
}

func TbFromJs(j string) *TbT {
	a := js.Ra(j)
	return NewTb(
		js.Ri(a[0]),
		js.Ri(a[1]),
		arr.Map(js.Ra(a[2]), FromJs),
	)
}

//  IDVAL TABLE

// Daily IdVal table
type TbIdValT struct {
	// Date of values
	Date string
	// Last values read
	Values []*IdValT
}

func NewTbIdVal(date string, values []*IdValT) *TbIdValT {
	return &TbIdValT{date, values}
}

func TbIdValToJs(t *TbIdValT) string {
	return js.Wa([]string{
		js.Ws(t.Date),
		js.Wa(arr.Map(t.Values, IdValToJs)),
	})
}

func TbIdValFromJs(j string) *TbIdValT {
	a := js.Ra(j)
	return NewTbIdVal(
		js.Rs(a[0]),
		arr.Map(js.Ra(a[1]), IdValFromJs),
	)
}
