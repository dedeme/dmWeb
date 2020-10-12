// Copyright 05-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Log data base.
package log

import (
	"github.com/dedeme/MrBackup/data/cts"
	"github.com/dedeme/MrBackup/data/logRow"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

var fpath string

// Auxiliar function
func write(a []json.T) {
	if len(a) > cts.LogMaxLength {
		a = a[len(a)-cts.LogMaxLength:]
	}
	file.WriteAll(fpath, json.Wa(a).String())
}

// Initializes log.
//    parent: Parent directory of "Log.tb".
func Initialize(parent string) {
	fpath = path.Join(parent, "Log.tb")
	if !file.Exists(fpath) {
		write([]json.T{})
	}
}

// Reinitializes log.
func Reset() {
	file.WriteAll(fpath, "[]")
}

// Read the content of log serialized in JSON.
func Read() json.T {
	return json.FromString(file.ReadAll(fpath))
}

// Adds an informative message to log.
//   msg: Message to show.
func Info(msg string) {
	a := Read().Ra()
	a = append(a, logRow.Info(msg).ToJs())
	write(a)
}

// Adds an error message to log.
//   msg: Message to show.
func Error(msg string) {
	a := Read().Ra()
	a = append(a, logRow.Error(msg).ToJs())
	write(a)
}
