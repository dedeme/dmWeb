// Copyright 14-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Log table.
package logTb

import (
	"github.com/dedeme/QMarket/data/log"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

var fpath string

// Initializes table
func Initialize(parent string) {
	fpath = path.Join(parent, "Log.tb")
	if !file.Exists(fpath) {
		Write(log.New())
	}
}

// Writes table.
func Write(l log.T) {
	file.WriteAll(fpath, l.ToJs().String())
}

// Reads table as JSON data.
func ReadJs() json.T {
	return json.FromString(file.ReadAll(fpath))
}

// Reads table
func Read() log.T {
	return log.FromJs(ReadJs())
}

// Reinitializes table.
func Reset() {
	Write(log.New())
}

// Adds an informative message to log.
//   msg: Message to show.
func Info(msg string) {
	Write(Read().AddInfo(msg))
}

// Adds an error message to log.
//   msg: Message to show.
func Error(msg string) {
	Write(Read().AddError(msg))
}
