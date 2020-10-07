// Copyright 15-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Log data base.
package log

import (
	"github.com/dedeme/MultiMarket/data/cts"
	"github.com/dedeme/MultiMarket/data/logRow"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

var fpath string

// Auxiliar function
func write(lk sync.T, a []json.T) {
	if len(a) > cts.LogMaxLength {
		a = a[len(a)-cts.LogMaxLength:]
	}
	file.WriteAll(fpath, json.Wa(a).String())
}

// Initializes log.
//    parent: Parent directory of "Log.tb".
func Initialize(lk sync.T, parent string) {
	fpath = path.Join(parent, "Log.tb")
	if !file.Exists(fpath) {
		write(lk, []json.T{})
	}
}

// Reinitializes log.
func Reset(lk sync.T) {
	file.WriteAll(fpath, "[]")
}

// Read the content of log serialized in JSON.
func Read(lk sync.T) json.T {
	return json.FromString(file.ReadAll(fpath))
}

// Adds an informative message to log.
//   lk : Synchronization lock.
//   msg: Message to show.
func Info(lk sync.T, msg string) {
	a := Read(lk).Ra()
	a = append(a, logRow.Info(msg).ToJs())
	write(lk, a)
}

// Adds an error message to log.
//   lk : Synchronization lock.
//   msg: Message to show.
func Error(lk sync.T, msg string) {
	a := Read(lk).Ra()
	a = append(a, logRow.Error(msg).ToJs())
	write(lk, a)
}
