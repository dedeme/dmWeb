// Copyright 05-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Log data base.
package log

import (
	"github.com/dedeme/MrBackup/data/cts"
	"github.com/dedeme/MrBackup/data/logRow"
	"github.com/dedeme/ktlib/file"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/path"
)

var fpath string

// Auxiliar function
func write(a []string) {
	if len(a) > cts.LogMaxLength {
		a = a[len(a)-cts.LogMaxLength:]
	}
	file.Write(fpath, js.Wa(a))
}

// Initializes log.
//    parent: Parent directory of "Log.tb".
func Initialize(parent string) {
	fpath = path.Cat(parent, "Log.tb")
	if !file.Exists(fpath) {
		write([]string{})
	}
}

// Reinitializes log.
func Reset() {
	file.Write(fpath, "[]")
}

// Read the content of log serialized in JSON.
func Read() string {
	return file.Read(fpath)
}

// Adds an informative message to log.
//   msg: Message to show.
func Info(msg string) {
	a := js.Ra(Read())
	a = append(a, logRow.Info(msg).ToJs())
	write(a)
}

// Adds an error message to log.
//   msg: Message to show.
func Error(msg string) {
	a := js.Ra(Read())
	a = append(a, logRow.Error(msg).ToJs())
	write(a)
}
