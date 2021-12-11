// Copyright 18-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Server data.
package server

import (
	"github.com/dedeme/QMarket/data/cts"
	"github.com/dedeme/QMarket/data/nick"
	"github.com/dedeme/golib/json"
)

// ConfT -----------------------------------------------------------------------

// Server configuration
type ConfT struct {
	cmd   string
	url   string
	regex string
	// Can be cts.ServerStopped, cts.ServerActive or cts.ServerSelected.
	sel           int
	isIsoDate     bool
	dateSeparator string
	isIsoNumber   bool
	/// In Diary are C-Q and in historic D-O-C-X-N-V
	fieldsType string
	/// Several hints are separated by |
	tableStart string
	/// Several hints are separated by |
	tableEnd string
	/// Several hints are separated by |
	rowStart string
	/// Several hints are separated by |
	rowEnd string
	/// Several hints are separated by |
	cellsStart []string
	/// Several hints are separated by |
	cellsEnd []string
}

// Returns extern command to read server.
func (cf *ConfT) Cmd() string {
	return cf.cmd
}

// Returns server URL
//   In hisotoric "${code}" will be replaced by the company code.
func (cf *ConfT) Url() string {
	return cf.url
}

// Returns expresion regular for reading page.
func (cf *ConfT) Regex() string {
	return cf.regex
}

// Returns server selection type.
//  Can be cts.ServerStopped, cts.ServerActive or cts.ServerSelected.
func (cf *ConfT) Sel() int {
	return cf.sel
}

// Returns if dates are type ISO.
func (cf *ConfT) IsIsoDate() bool {
	return cf.isIsoDate
}

// Returns date separator.
func (cf *ConfT) DateSeparator() string {
	return cf.dateSeparator
}

// Returns if numbers value are type ISO.
func (cf *ConfT) IsIsoNumber() bool {
	return cf.isIsoNumber
}

// Order and type of fields in each record.
//    In Diary are C-Q and in historic D-O-C-X-N-V
func (cf *ConfT) FieldsType() string {
	return cf.fieldsType
}

// Returns hints separates for '|' to table start.
func (cf *ConfT) TableStart() string {
	return cf.tableStart
}

// Returns hints separates for '|' to table end.
func (cf *ConfT) TableEnd() string {
	return cf.tableEnd
}

// Returns hints separates for '|' to row start.
func (cf *ConfT) RowStart() string {
	return cf.rowStart
}

// Returns hints separates for '|' to row end.
func (cf *ConfT) RowEnd() string {
	return cf.rowEnd
}

// Returns hints separates for '|' to cell start.
func (cf *ConfT) CellsStart() []string {
	return cf.cellsStart
}

// Returns hints separates for '|' to cell end.
func (cf *ConfT) CellsEnd() []string {
	return cf.cellsEnd
}

func (cf *ConfT) toJs() json.T {
	var cstart []json.T
	for _, e := range cf.cellsStart {
		cstart = append(cstart, json.Ws(e))
	}
	var cend []json.T
	for _, e := range cf.cellsEnd {
		cend = append(cend, json.Ws(e))
	}
	return json.Wa([]json.T{
		json.Ws(cf.cmd),
		json.Ws(cf.url),
		json.Ws(cf.regex),
		json.Wi(cf.sel),
		json.Wb(cf.isIsoDate),
		json.Ws(cf.dateSeparator),
		json.Wb(cf.isIsoNumber),
		json.Ws(cf.fieldsType),
		json.Ws(cf.tableStart),
		json.Ws(cf.tableEnd),
		json.Ws(cf.rowStart),
		json.Ws(cf.rowEnd),
		json.Wa(cstart),
		json.Wa(cend),
	})
}

func confFromJs(js json.T) *ConfT {
	a := js.Ra()
	var cstart []string
	for _, e := range a[12].Ra() {
		cstart = append(cstart, e.Rs())
	}
	var cend []string
	for _, e := range a[13].Ra() {
		cend = append(cend, e.Rs())
	}
	return &ConfT{
		a[0].Rs(),
		a[1].Rs(),
		a[2].Rs(),
		a[3].Ri(),
		a[4].Rb(),
		a[5].Rs(),
		a[6].Rb(),
		a[7].Rs(),
		a[8].Rs(),
		a[9].Rs(),
		a[10].Rs(),
		a[11].Rs(),
		cstart,
		cend,
	}
}

// CodeT -----------------------------------------------------------------------

// Entry in server codes list.
type CodeT struct {
	nickId int
	// Optional.
	code *string
}

// Identifier of nick.
func (cd *CodeT) NickId() int {
	return cd.nickId
}

// Code for nick or 'ok==false' if such nick has no code or its code is "".
func (cd *CodeT) Code() (c string, ok bool) {
	if cd.code != nil {
		c = *cd.code
		if c != "" {
			ok = true
		}
	}
	return
}

func (cd *CodeT) toJs() json.T {
	c := json.Wn()
	if cd.code != nil {
		c = json.Ws(*cd.code)
	}
	return json.Wa([]json.T{
		json.Wi(cd.nickId),
		c,
	})
}

func codeFromJs(js json.T) *CodeT {
	a := js.Ra()
	var c *string
	if !a[1].IsNull() {
		s := a[1].Rs()
		c = &s
	}
	return &CodeT{
		a[0].Ri(),
		c,
	}
}

// T ---------------------------------------------------------------------------

type T struct {
	id        int
	shortName string
	name      string
	// Optional.
	dailyConf *ConfT
	// Optional.
	historicConf *ConfT
	codes        []*CodeT
}

// Constructor
//    id   : Identifier.
//    name : Used for shortName and name.
//    nicks: Nick list from 'nicksTb.Nicks()'
func New(id int, name string, nicks []*nick.T) *T {
	var codes []*CodeT
	for _, e := range nicks {
		codes = append(codes, &CodeT{e.Id(), nil})
	}
	return &T{
		id,
		name,
		name,
		nil,
		nil,
		codes,
	}
}

// Returns server identifier.
func (s *T) Id() int {
	return s.id
}

// Returns server short name.
func (s *T) ShortName() string {
	return s.shortName
}

// Returns server name.
func (s *T) Name() string {
	return s.name
}

// Returns daily configuration or "ok==false" if it does not exist.
func (s *T) DailyConf() (cf *ConfT, ok bool) {
	if s.dailyConf != nil {
		cf = s.dailyConf
		ok = true
	}
	return
}

// Returns historic configuration or "ok==false" if it does not exist.
func (s *T) HistoricConf() (cf *ConfT, ok bool) {
	if s.historicConf != nil {
		cf = s.historicConf
		ok = true
	}
	return
}

// Returns list of nick-code
func (s *T) Codes() []*CodeT {
	return s.codes
}

// Adds a new nick code to 'server.Codes()' if it does not exist.
//    nickId: Identifier of nick to update.
func (s *T) AddCode(nickId int) {
	for _, e := range s.codes {
		if e.nickId == nickId {
			return
		}
	}
	s.codes = append(s.codes, &CodeT{nickId, nil})
}

// Removes a nick from 'server.Codes()' if it exists.
//    nickId: Identifier of nick to update.
func (s *T) RemoveCode(nickId int) {
	var codes []*CodeT
	for _, e := range s.codes {
		if e.nickId == nickId {
			continue
		}
		codes = append(codes, e)
	}
	s.codes = codes
}

// Modifies the code of a nick in 'server.Codes()' if it exists.
//    nickId: Identifier of nick to update.
//    code  : New nick server code.
func (s *T) ModifyCode(nickId int, code string) {
	var codes []*CodeT
	for _, e := range s.codes {
		if e.nickId == nickId {
			codes = append(codes, &CodeT{nickId, &code})
			continue
		}
		codes = append(codes, e)
	}
	s.codes = codes
}

func (s *T) ToJs() json.T {
	dconf := json.Wn()
	if s.dailyConf != nil {
		dconf = s.dailyConf.toJs()
	}
	hconf := json.Wn()
	if s.historicConf != nil {
		hconf = s.historicConf.toJs()
	}
	var codes []json.T
	for _, e := range s.codes {
		codes = append(codes, e.toJs())
	}
	return json.Wa([]json.T{
		json.Wi(s.id),
		json.Ws(s.shortName),
		json.Ws(s.name),
		dconf,
		hconf,
		json.Wa(codes),
	})
}

func FromJs(js json.T) *T {
	a := js.Ra()
	var dconf *ConfT
	if !a[3].IsNull() {
		dconf = confFromJs(a[3])
	}
	var hconf *ConfT
	if !a[4].IsNull() {
		hconf = confFromJs(a[4])
	}
	var codes []*CodeT
	for _, e := range a[5].Ra() {
		codes = append(codes, codeFromJs(e))
	}
	return &T{
		a[0].Ri(),
		a[1].Rs(),
		a[2].Rs(),
		dconf,
		hconf,
		codes,
	}
}

// TableT ----------------------------------------------------------------------

// Server table structure
type TableT struct {
	nextId int
	list   []*T
}

func NewTable() *TableT {
	return &TableT{0, []*T{}}
}

// Returns the list of servers
func (t *TableT) List() []*T {
	return t.list
}

// Adds a new server in place. If shortName is duplicated, it returns 'false'.
//    shortName: Server short name.
//    nicks: Nick list from 'nicksTb.Nicks()'
func (t *TableT) Add(shortName string, nicks []*nick.T) bool {
	for _, e := range t.list {
		if e.ShortName() == shortName {
			return false
		}
	}

	t.list = append(t.list, New(t.nextId, shortName, nicks))
	t.nextId++
	return true
}

// Modify data of "sv" in place. If there is not a server with 'sv.id', function
// modifies nothing and returns 'ok=false'
//    sv: Server to modify.
func (t *TableT) Modify(sv *T) (ok bool) {
	id := sv.Id()
	var list []*T
	for _, e := range t.list {
		if e.Id() == id {
			list = append(list, sv)
			ok = true
			continue
		}
		list = append(list, e)
	}
	if ok {
		t.list = list
	}

	return
}

// Deletes a server in place. If there is not a server with 'id', function does
// nothing and returns 'ok=false'
//    id: Identifier of server to modify.
func (t *TableT) Del(id int) (ok bool) {
	var list []*T
	for _, e := range t.list {
		if e.Id() == id {
			ok = true
			continue
		}
		list = append(list, e)
	}
	if ok {
		t.list = list
	}

	return
}

// Returns the list of activated-selected historic configuration servers and
// index of the selected server.
func (t *TableT) DailyList() (svs []*T, selectedIx int) {
	ix := 0
	for _, e := range t.list {
		cf, ok := e.DailyConf()
		if ok && cf.Sel() != cts.ServerStopped {
			svs = append(svs, e)
			if cf.Sel() == cts.ServerSelected {
				selectedIx = ix
			}
			ix++
		}
	}
	return
}

// Returns the list of activated-selected historic configuration servers and
// index of the selected server.
func (t *TableT) HistoricList() (svs []*T, selectedIx int) {
	ix := 0
	for _, e := range t.list {
		cf, ok := e.HistoricConf()
		if ok && cf.Sel() != cts.ServerStopped {
			svs = append(svs, e)
			if cf.Sel() == cts.ServerSelected {
				selectedIx = ix
			}
			ix++
		}
	}
	return
}

// Modifies in place 't' adding nickId from servers in witch it is not included.
func (t *TableT) NickAdd(nickId int) {
	for _, s := range t.list {
		s.AddCode(nickId)
	}
}

// Modifies in place 't' removeing nickId from every server
func (t *TableT) NickRemove(nickId int) {
	for _, s := range t.list {
		s.RemoveCode(nickId)
	}
}

// Modifies in place the code of 'nickId' in 'serverId'
//    serverId: Identifier of server to modify.
//    nickId  : Identifier of nick to modify.
//    code    : New nick server code.
func (t *TableT) ModifyNickCode(serverId, nickId int, code string) {
	for _, s := range t.list {
		if s.id == serverId {
			cds := s.codes
			for i := range cds {
				if cds[i].nickId == nickId {
					cds[i] = &CodeT{nickId, &code}
					break
				}
			}
			break
		}
	}
}

func (t *TableT) ToJs() json.T {
	var l []json.T
	for _, e := range t.list {
		l = append(l, e.ToJs())
	}
	return json.Wa([]json.T{
		json.Wi(t.nextId),
		json.Wa(l),
	})
}

func TableFromJs(js json.T) *TableT {
	a := js.Ra()
	var l []*T
	for _, e := range a[1].Ra() {
		l = append(l, FromJs(e))
	}
	return &TableT{
		a[0].Ri(),
		l,
	}
}
