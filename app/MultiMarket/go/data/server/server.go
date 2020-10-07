// Copyright 23-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Server data.
package server

import (
	"github.com/dedeme/MultiMarket/data/nick"
	"github.com/dedeme/golib/json"
)

type ConfT struct {
	// Extern command to read server.
	cmd string
	// Server URL
	url string
	// In hisotoric "${code}" will be replaced by the company code.
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

func (cf *ConfT) Cmd() string {
	return cf.cmd
}

func (cf *ConfT) Url() string {
	return cf.url
}

func (cf *ConfT) Regex() string {
	return cf.regex
}

func (cf *ConfT) Sel() int {
	return cf.sel
}

func (cf *ConfT) IsIsoDate() bool {
	return cf.isIsoDate
}

func (cf *ConfT) DateSeparator() string {
	return cf.dateSeparator
}

func (cf *ConfT) IsIsoNumber() bool {
	return cf.isIsoNumber
}

func (cf *ConfT) FieldsType() string {
	return cf.fieldsType
}

func (cf *ConfT) TableStart() string {
	return cf.tableStart
}

func (cf *ConfT) TableEnd() string {
	return cf.tableEnd
}

func (cf *ConfT) RowStart() string {
	return cf.rowStart
}

func (cf *ConfT) RowEnd() string {
	return cf.rowEnd
}

func (cf *ConfT) CellsStart() []string {
	return cf.cellsStart
}

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

type CodeT struct {
	nickId int
	// Option.
	code *string
}

func (cd *CodeT) NickId() int {
	return cd.nickId
}

func (cd *CodeT) Code() (c string, ok bool) {
	if cd.code != nil {
		c = *cd.code
		if c != "" {
			ok = true
		}
	}
	return
}

type T struct {
	id        int
	shortName string
	name      string
	// Option.
	dailyConf *ConfT
	// Option
	historicConf *ConfT
	codes        []*CodeT
}

func (s *T) Id() int {
	return s.id
}

func (s *T) ShortName() string {
	return s.shortName
}

func (s *T) Name() string {
	return s.name
}

func (s *T) DailyConf() (cf *ConfT, ok bool) {
	if s.dailyConf != nil {
		cf = s.dailyConf
		ok = true
	}
	return
}

func (s *T) HistoricConf() (cf *ConfT, ok bool) {
	if s.historicConf != nil {
		cf = s.historicConf
		ok = true
	}
	return
}

func (s *T) Codes() []*CodeT {
	return s.codes
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
