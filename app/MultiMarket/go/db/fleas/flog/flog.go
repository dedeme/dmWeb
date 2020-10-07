// Copyright 10-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Fleas log.
package flog

import (
	"github.com/dedeme/MultiMarket/data/logRow"
	"github.com/dedeme/golib/date"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

var fpath string
var fid string

// Auxiliar function
func write(log []*logRow.T) {
	var a []json.T
	for _, e := range log {
		a = append(a, e.ToJs())
	}
	file.WriteAll(fpath, json.Wa(a).String())
}

// Auxiliar function.
func read() []*logRow.T {
	var a []*logRow.T
	for _, e := range json.FromString(file.ReadAll(fpath)).Ra() {
		a = append(a, logRow.FromJs(e))
	}
	return a
}

// Initializes log.
//    parent: Parent directory of "Flog.tb".
func Initialize(parent string) {
	fpath = path.Join(parent, "Flog.tb")
	if !file.Exists(fpath) {
		write([]*logRow.T{})
	}
}

// Resets flog (Delete its contents), generates a new id and returns it.
func NewId() string {
	write([]*logRow.T{})
	fid = date.Now().Format("%T")
	return fid
}

// Returns 'true' if "id" is correct.
//    id: Log identifier control.
func Check(id string) bool {
	return id != "" && id == fid
}

// Stops flog.
//    id: Log identifier control. If 'id' is outdated this function do nothing.
func Stop(id string) {
	if id != "" && id == fid {
		fid = ""
	}
}

// Adds a normal message.
//    id : Log identifier control. If 'id' is outdated this function do nothing.
//    msg: Message to show.
func Info(id, msg string) {
	if id != "" && id == fid {
		es := read()
		write(append([]*logRow.T{logRow.Info(msg)}, es...))
	}
}

// Reads Log and returns an 'JSONized' Arr[logRow.T], ordered from after to
// before.
//    id : Log identifier control. If 'id' is outdated 'ok==false'.
func Read(id string) (log json.T, ok bool) {
	if id != "" && id == fid {
		log = json.FromString(file.ReadAll(fpath))
		ok = true
	}
	return
}
