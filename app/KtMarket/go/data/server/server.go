// Copyright 18-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Server data.
package server

import (
	"github.com/dedeme/KtMarket/cts"
	"github.com/dedeme/KtMarket/data/nick"
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/sys"
)

// ConfT -----------------------------------------------------------------------

// Server configuration
type ConfT struct {
	// Extern command to read server.
	Cmd string
	// Server URL
	//   In hisotoric "${code}" will be replaced by the company code.
	Url string
	// Expresion regular for reading page. Tiene la forma:
	//    expression|x|replacement
	// For example
	//    lar_esp.*"|x|lar_espana"
	Regex string
	// server selection type.
	//   Can be cts.ServerStopped, cts.ServerActive or cts.ServerSelected.
	Sel int
	// 'true' if dates are type ISO.
	IsIsoDate bool
	// Date separator
	DateSeparator string
	// 'true' if number values are type ISO.
	IsIsoNumber bool
	// Order and type of fields in each record.
	//    In Diary are C-Q and in historic D-O-C-X-N-V
	FieldsType string
	/// Several hints are separated by |
	TableStart string
	/// Several hints are separated by |
	TableEnd string
	/// Several hints are separated by |
	RowStart string
	/// Several hints are separated by |
	RowEnd string
	/// Several hints are separated by |
	CellsStart []string
	/// Several hints are separated by |
	CellsEnd []string
}

func confToJs(cf *ConfT) string {
	return js.Wa([]string{
		js.Ws(cf.Cmd),
		js.Ws(cf.Url),
		js.Ws(cf.Regex),
		js.Wi(cf.Sel),
		js.Wb(cf.IsIsoDate),
		js.Ws(cf.DateSeparator),
		js.Wb(cf.IsIsoNumber),
		js.Ws(cf.FieldsType),
		js.Ws(cf.TableStart),
		js.Ws(cf.TableEnd),
		js.Ws(cf.RowStart),
		js.Ws(cf.RowEnd),
		js.Wa(arr.Map(cf.CellsStart, func(c string) string {
			return js.Ws(c)
		})),
		js.Wa(arr.Map(cf.CellsEnd, func(c string) string {
			return js.Ws(c)
		})),
	})
}

func confFromJs(j string) *ConfT {
	a := js.Ra(j)
	return &ConfT{
		js.Rs(a[0]),
		js.Rs(a[1]),
		js.Rs(a[2]),
		js.Ri(a[3]),
		js.Rb(a[4]),
		js.Rs(a[5]),
		js.Rb(a[6]),
		js.Rs(a[7]),
		js.Rs(a[8]),
		js.Rs(a[9]),
		js.Rs(a[10]),
		js.Rs(a[11]),
		arr.Map(js.Ra(a[12]), func(j string) string {
			return js.Rs(j)
		}),
		arr.Map(js.Ra(a[13]), func(j string) string {
			return js.Rs(j)
		}),
	}
}

// CodeT -----------------------------------------------------------------------

// Entry in server codes list.
type CodeT struct {
	// Nick identifier
	NickId int
	//Returns server code for nick or 'ok==false' if such nick has not one.
	Code sys.OpT[string]
}

func newCode(nickId int) *CodeT {
	return &CodeT{
		nickId,
		sys.None[string](),
	}
}

func codeToJs(cd *CodeT) string {
	cjs := js.Wn()
	if c, ok := cd.Code(); ok {
		cjs = js.Ws(c)
	}
	return js.Wa([]string{
		js.Wi(cd.NickId),
		cjs,
	})
}

func codeFromJs(j string) *CodeT {
	a := js.Ra(j)

	c := sys.None[string]()
	if !js.IsNull(a[1]) {
		c = sys.Some(js.Rs(a[1]))
	}

	return &CodeT{
		js.Ri(a[0]),
		c,
	}
}

// T ---------------------------------------------------------------------------

type T struct {
	Id        int
	ShortName string
	Name      string
	// Optional daily configuration
	DailyConf sys.OpT[*ConfT]
	// Optional historic configuration or "ok==false" if it does not exist.
	HistoricConf sys.OpT[*ConfT]
	// List of nickId-serverCode
	Codes []*CodeT
}

// Constructor
//    id   : Identifier.
//    name : Used for shortName and name.
//    nicks: Nick list from 'nicksTb.Nicks()'
func New(id int, name string, nicks []*nick.T) *T {
	return &T{
		id,
		name,
		name,
		sys.None[*ConfT](),
		sys.None[*ConfT](),
		arr.Map(nicks, func(nk *nick.T) *CodeT {
			return newCode(nk.Id)
		}),
	}
}

// Adds a new nick code to 'server.Codes()' if it does not exist.
//    nickId: Identifier of nick to update.
func (s *T) AddCode(nickId int) {
	if !arr.Anyf(s.Codes, func(cd *CodeT) bool {
		return cd.NickId == nickId
	}) {
		s.Codes = append(s.Codes, newCode(nickId))
	}
}

// Removes a nick from 'server.Codes()' if it exists.
//    nickId: Identifier of nick to update.
func (s *T) RemoveCode(nickId int) {
	ix := arr.Indexf(s.Codes, func(cd *CodeT) bool {
		return cd.NickId == nickId
	})
	if ix != -1 {
		arr.Remove(&s.Codes, ix)
	}
}

// Modifies the code of a nick in 'server.Codes()' if it exists.
//    nickId: Identifier of nick to update.
//    code  : New nick server code.
func (s *T) ModifyCode(nickId int, code string) {
	c, ok := arr.Find(s.Codes, func(cd *CodeT) bool {
		return cd.NickId == nickId
	})
	if ok {
		c.Code = sys.Some(code)
	}
}

func ToJs(s *T) string {
	djs := js.Wn()
	if dcf, ok := s.DailyConf(); ok {
		djs = confToJs(dcf)
	}
	hjs := js.Wn()
	if hcf, ok := s.HistoricConf(); ok {
		hjs = confToJs(hcf)
	}
	return js.Wa([]string{
		js.Wi(s.Id),
		js.Ws(s.ShortName),
		js.Ws(s.Name),
		djs,
		hjs,
		js.Wa(arr.Map(s.Codes, func(c *CodeT) string {
			return codeToJs(c)
		})),
	})
}

func FromJs(j string) *T {
	a := js.Ra(j)

	ccf := sys.None[*ConfT]()
	if !js.IsNull(a[3]) {
		ccf = sys.Some(confFromJs(a[3]))
	}
	hcf := sys.None[*ConfT]()
	if !js.IsNull(a[4]) {
		hcf = sys.Some(confFromJs(a[4]))
	}

	return &T{
		js.Ri(a[0]),
		js.Rs(a[1]),
		js.Rs(a[2]),
		ccf,
		hcf,
		arr.Map(js.Ra(a[5]), func(j string) *CodeT {
			return codeFromJs(j)
		}),
	}
}

// TbT ----------------------------------------------------------------------

// Server table structure
type TbT struct {
	// Next identifier
	NextId int
	// List of servers
	List []*T
}

func NewTb() *TbT {
	return &TbT{0, []*T{}}
}

// Adds a new server in place. If shortName is duplicated, it returns 'false'.
//    shortName: Server short name.
//    nicks: Nick list from 'nicksTb.Nicks()'
func (t *TbT) Add(shortName string, nicks []*nick.T) bool {
	if arr.Anyf(t.List, func(s *T) bool {
		return s.ShortName == shortName
	}) {
		return false
	}

	t.List = append(t.List, New(t.NextId, shortName, nicks))
	t.NextId++
	return true
}

// Modify data of "sv" in place. If there is not a server with 'sv.id', function
// modifies nothing and returns 'ok=false'
//    sv: Server to modify.
func (t *TbT) Modify(sv *T) (ok bool) {
	id := sv.Id
	var List []*T
	for _, e := range t.List {
		if e.Id == id {
			List = append(List, sv)
			ok = true
			continue
		}
		List = append(List, e)
	}
	if ok {
		t.List = List
	}

	return
}

// Deletes a server in place. If there is not a server with 'id', function does
// nothing and returns 'ok=false'
//    id: Identifier of server to modify.
func (t *TbT) Del(id int) (ok bool) {
	var list []*T
	for _, e := range t.List {
		if e.Id == id {
			ok = true
			continue
		}
		list = append(list, e)
	}
	if ok {
		t.List = list
	}

	return
}

// Returns the list of activated-selected historic configuration servers and
// index of the selected server.
func (t *TbT) DailyList() (svs []*T, selectedIx int) {
	ix := 0
	for _, e := range t.List {
		cf, ok := e.DailyConf()
		if ok && cf.Sel != cts.ServerStopped {
			svs = append(svs, e)
			if cf.Sel == cts.ServerSelected {
				selectedIx = ix
			}
			ix++
		}
	}
	return
}

// Returns the list of activated-selected historic configuration servers and
// index of the selected server.
func (t *TbT) HistoricList() (svs []*T, selectedIx int) {
	ix := 0
	for _, e := range t.List {
		cf, ok := e.HistoricConf()
		if ok && cf.Sel != cts.ServerStopped {
			svs = append(svs, e)
			if cf.Sel == cts.ServerSelected {
				selectedIx = ix
			}
			ix++
		}
	}
	return
}

// Modifies in place 't' adding nickId from servers in witch it is not included.
func (t *TbT) NickAdd(nickId int) {
	for _, s := range t.List {
		s.AddCode(nickId)
	}
}

// Modifies in place 't' removeing nickId from every server
func (t *TbT) NickRemove(nickId int) {
	for _, s := range t.List {
		s.RemoveCode(nickId)
	}
}

// Modifies in place the code of 'nickId' in 'serverId'
//    serverId: Identifier of server to modify.
//    nickId  : Identifier of nick to modify.
//    code    : New nick server code.
func (t *TbT) ModifyNickCode(serverId, nickId int, code string) {
	sv, ok := arr.Find(t.List, func(s *T) bool {
		return s.Id == serverId
	})
	if ok {
		sv.ModifyCode(nickId, code)
	}
}

func TbToJs(t *TbT) string {
	return js.Wa([]string{
		js.Wi(t.NextId),
		js.Wa(arr.Map(t.List, func(s *T) string {
			return ToJs(s)
		})),
	})
}

func TbFromJs(j string) *TbT {
	a := js.Ra(j)
	return &TbT{
		js.Ri(a[0]),
		arr.Map(js.Ra(a[1]), func(j string) *T {
			return FromJs(j)
		}),
	}
}
