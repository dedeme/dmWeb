// Copyright 08-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Manager data.
package manager

import (
	"github.com/dedeme/MultiMarket/data/flea/fmodel"
	"github.com/dedeme/MultiMarket/data/flea/fmodels"
	"github.com/dedeme/golib/json"
)

type EntryT struct {
	model  *fmodel.T
	params []float64
}

// Creates a default manager entry.
func NewEntry() *EntryT {
	md := fmodels.List()[0]
	var params []float64
	mins := md.ParMins()
	maxs := md.ParMaxs()
	for i := 0; i < len(maxs); i++ {
		params = append(params, (mins[i]+maxs[i])/float64(2))
	}
	return &EntryT{md, params}
}

// Creates a manager entry from values.
func NewEntryFrom(model *fmodel.T, params []float64) *EntryT {
	return &EntryT{model, params}
}

func (e *EntryT) Model() *fmodel.T {
	return e.model
}

func (e *EntryT) Params() []float64 {
	return e.params
}

// Two entry are equals if they have equals 'model' and 'params'.
//    e2: Other entry
func (e *EntryT) Eq(e2 *EntryT) bool {
	if e.model.Id() == e2.model.Id() {
		for i, p := range e.params {
			df := p - e2.params[i]
			if df > 0.000001 || df < -0.000001 {
				return false
			}
		}
		return true
	}
	return false
}

// Returns an entry JSONized to save in file system.
func (e *EntryT) ToJs() json.T {
	var paramsJs []json.T
	for _, e := range e.params {
		paramsJs = append(paramsJs, json.Wd(e))
	}
	return json.Wa([]json.T{
		json.Ws(e.model.Id()),
		json.Wa(paramsJs),
	})
}

// Returns an entry JSONized for sending to client (javascript).
func (e *EntryT) ToJsClient() json.T {
	var paramsJs []json.T
	for _, e := range e.params {
		paramsJs = append(paramsJs, json.Wd(e))
	}
	return json.Wa([]json.T{
		e.model.ToJs(),
		json.Wa(paramsJs),
	})
}

// Returns an entry from a JSON value generated with 'entryTojs'. If entry model
// is not found, a default entry is returned.
func EntryFromJs(js json.T) *EntryT {
	a := js.Ra()
	model, ok := fmodels.GetModel(a[0].Rs())
	if !ok {
		return NewEntry()
	}
	var params []float64
	for _, e := range a[1].Ra() {
		params = append(params, e.Rd())
	}
	return &EntryT{model, params}
}

type T struct {
	Base  *EntryT
	nicks map[string]*EntryT
}

func New() *T {
	return &T{NewEntry(), map[string]*EntryT{}}
}

func (m *T) Nicks() map[string]*EntryT {
	return m.nicks
}

// Returns the model and parameters of a nick.
//    nick: Nick name to search.
func (m *T) GetModel(nick string) *EntryT {
	e, ok := m.nicks[nick]
	if !ok {
		e = NewEntry()
	}
	return e
}

// Returns a manager JSONized to save in file system.
func (m *T) ToJs() json.T {
	nicksJs := map[string]json.T{}
	for k, v := range m.nicks {
		nicksJs[k] = v.ToJs()
	}
	return json.Wa([]json.T{
		m.Base.ToJs(),
		json.Wo(nicksJs),
	})
}

// Returns a manager JSONized for sending to client (javascript).
func (m *T) ToJsClient() json.T {
	nicksJs := map[string]json.T{}
	for k, v := range m.nicks {
		nicksJs[k] = v.ToJsClient()
	}
	return json.Wa([]json.T{
		m.Base.ToJsClient(),
		json.Wo(nicksJs),
	})
}

// Returns a manager from a JSON value generated with 'tojs'.
func FromJs(js json.T) *T {
	a := js.Ra()
	nicks := map[string]*EntryT{}
	for k, v := range a[1].Ro() {
		nicks[k] = EntryFromJs(v)
	}
	return &T{
		EntryFromJs(a[0]),
		nicks,
	}
}
