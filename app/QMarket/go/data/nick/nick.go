// Copyright 18-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Nick data.
package nick

import (
	"github.com/dedeme/golib/json"
)

type T struct {
	id    int
	name  string
	isSel bool
}

// Constructor.
//    id   : Identifier.
//    name : Name (TEF, ACS...).
//    isSel: 'true' if it is selected to operate.
func New(id int, name string, isSel bool) *T {
	return &T{id, name, isSel}
}

// Identifier.
func (n *T) Id() int {
	return n.id
}

// Name (TEF, ACS...).
func (n *T) Name() string {
	return n.name
}

// If the nick is selected to operate.
func (n *T) IsSel() bool {
	return n.isSel
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

// QvalueT --------------------------------------------------------------------

// Container nick - value
type QvalueT struct {
	// Nick id.
	nick int
	// A quote value: open, close, max, min, vol.
	value float64
}

// Constructor
//    nk   : Nick id.
//    value: A quote value: open, close, max, min, vol.
func NewQvalue(nk int, value float64) *QvalueT {
	return &QvalueT{nk, value}
}

// Returns nick id
func (qv *QvalueT) Nick() int {
	return qv.nick
}

// Returns a quote value: open, close, max, min or vol.
func (qv *QvalueT) Value() float64 {
	return qv.value
}

func (qv *QvalueT) ToJs() json.T {
	return json.Wa([]json.T{
		json.Wi(qv.nick),
		json.Wd(qv.value),
	})
}

func QvalueFromJs(js json.T) *QvalueT {
	a := js.Ra()
	return &QvalueT{
		a[0].Ri(),
		a[1].Rd(),
	}
}

// TableT ----------------------------------------------------------------------

// Nicks table structure
type TableT struct {
	nextId int
	model  *int
	list   []*T
}

// Create a table without nicks.
func NewTable() *TableT {
	return &TableT{0, nil, []*T{}}
}

// Return the id of nick used as model or "ok==false' if it does not exist.
func (t *TableT) Model() (nickId int, ok bool) {
	if t.model != nil {
		nickId = *t.model
		ok = true
	}
	return
}

// Returns every nick list.
func (t *TableT) List() []*T {
	return t.list
}

// Returns the selected nicks.
func (t *TableT) SelectedNicks() []*T {
	var r []*T
	for _, n := range t.list {
		if n.isSel {
			r = append(r, n)
		}
	}
	return r
}

// Returns a nick from its 'id' or "ok==false"
func (t *TableT) NickFromId(id int) (nk *T, ok bool) {
	for _, nk = range t.list {
		if nk.id == id {
			ok = true
			return
		}
	}
	return
}

// Add a new nick if there is no one with the same name, otherwise returns
// 'ok==false'
//    t : Table to add a new nick.
//    nickName: Nick name (TEF, BBVA, ...)
//    RETURN:
//      newT: A new table after add 'nk'.
//      nk  : The new nick added.
//      ok  : 'false' if nickName is duplicated.
func (t *TableT) Add(nickName string) (newT *TableT, nk *T, ok bool) {
	id := t.nextId
	nk = New(id, nickName, false)
	for _, n := range t.list {
		if n.Name() == nickName {
			return
		}
	}
	newT = &TableT{t.nextId + 1, t.model, append(t.list, nk)}
	ok = true
	return
}

// Removes a nick if it exists
func (t *TableT) Del(nickId int) *TableT {
	var nks []*T
	for _, n := range t.List() {
		if n.id != nickId {
			nks = append(nks, n)
		}
	}
	md := t.model
	if md != nil && *md == nickId {
		md = nil
	}

	return &TableT{t.nextId, md, nks}
}

// Modify name of 'nickId'.
//    nickId: Identifier of nick to change.
//    name: new name.
//    Return:
//      oldName: The changed name. If the new name is equals to the old name,
//               the function do nothing.
//      ok     : Return false if 'name' is duplicated.
func (t *TableT) Modify(nickId int, name string) (oldName string, ok bool) {
	nk, idOk := t.NickFromId(nickId)
	if !idOk || nk.name == name {
		ok = true
		return
	}

	ix := -1
	for i, n := range t.List() {
		if n.id == nickId {
			ix = i
			oldName = n.name
		} else if n.name == name {
			return
		}
	}

	t.List()[ix] = New(nickId, name, nk.isSel)
	ok = true
	return
}

// Set value 'isSel' of a nick if it exists
func (t *TableT) SetSel(nickId int, isSel bool) *TableT {
	var nks []*T
	for _, n := range t.List() {
		if n.id == nickId {
			nks = append(nks, New(nickId, n.name, isSel))
			continue
		}
		nks = append(nks, n)
	}

	return &TableT{t.nextId, t.model, nks}
}

// Set 'nickId' as nick model if it exists
func (t *TableT) SetModel(nickId int) *TableT {
	_, ok := t.NickFromId(nickId)
	if !ok {
		nickId = *t.model
	}
	return &TableT{t.nextId, &nickId, t.list}
}

func (t *TableT) ToJs() json.T {
	modelJs := json.Wn()
	if t.model != nil {
		modelJs = json.Wi(*t.model)
	}
	var listJs []json.T
	for _, n := range t.list {
		listJs = append(listJs, n.ToJs())
	}
	return json.Wa([]json.T{
		json.Wi(t.nextId),
		modelJs,
		json.Wa(listJs),
	})
}

func TableFromJs(js json.T) *TableT {
	a := js.Ra()
	var model *int
	if !a[1].IsNull() {
		md := a[1].Ri()
		model = &md
	}
	var lst []*T
	for _, j := range a[2].Ra() {
		lst = append(lst, FromJs(j))
	}
	return &TableT{
		a[0].Ri(),
		model,
		lst,
	}
}
