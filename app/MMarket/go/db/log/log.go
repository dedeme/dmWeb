// Copyright 04-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Application logger
package log

import (
	"fmt"
	"github.com/dedeme/MMarket/data/cts"
	"github.com/dedeme/MMarket/data/logRow"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"github.com/dedeme/golib/date"
	gpath "path"
	"runtime/debug"
)

var path string

func now() string {
	dt := date.Now()
	return dt.Format("%D/%M/%Y(%t)")
}

// Initializes log.db.
//    dir: Parent directory.
func Initialize(dir string) {
	path = gpath.Join(dir, "log.db")
	if !file.Exists(path) {
		Reset()
	}
}

func add(msg logRow.T) {
	a := Read().Ra()
	for len(a) > cts.LOG_MAX_ENTRIES {
		a = a[1:]
	}
	a = append(a, msg.ToJs())
	file.WriteAll(path, json.Wa(a).String())
}

// Remove every entry of log.
func Reset() {
	file.WriteAll(path, json.Wa([]json.T{}).String())
}

// Adds a error message, addyng stack trace.
func Error(msg string) {
	add(logRow.NewError(now(), fmt.Sprintf("%v\n%v", msg, string(debug.Stack()))))
}

// Adds a error message.
func Info(msg string) {
	add(logRow.NewInfo(now(), msg))
}

// Read log in JSON format.
//    - Order is from before to after.
func Read() json.T {
	return json.FromString(file.ReadAll(path))
}
