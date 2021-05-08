// Copyright 13-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Log row.
package logRow

import (
	"github.com/dedeme/golib/date"
	"github.com/dedeme/golib/json"
	"runtime/debug"
	"strings"
)

type T struct {
	error bool
	time  string
	msg   string
}

// Current date in format "log".
func now() string {
	return date.Now().Format("%D/%M/%Y(%t)")
}

// Adds the stack trace to a message.
//
// This function relies on a stack line containing: "src/runtime/panic.go:".
//    msg: Message to format.
func Format(msg string) string {
	var bf strings.Builder
	bf.WriteString(msg)
	s := (string(debug.Stack()))
	a := strings.Split(s, "\n")
	limit := 10
	delay := true
	for i, s := range a {
		if delay {
			if strings.Index(s, "src/runtime/panic.go:") == -1 {
				limit++
				continue
			}
			delay = false
			continue
		}
		bf.WriteString("\n  " + strings.ReplaceAll(s, "\t", "  "))
		if i >= limit {
			break
		}
	}
	return bf.String()
}

// Creates an informative message.
//    msg: Message to show.
func Info(msg string) *T {
	return &T{false, now(), msg}
}

// Creates an error message with stack trace.
//    msg: Message to show.
func Error(msg string) *T {
	return &T{true, now(), Format(msg)}
}

func (r *T) ToJs() json.T {
	return json.Wa([]json.T{
		json.Wb(r.error),
		json.Ws(r.time),
		json.Ws(r.msg),
	})
}

func FromJs(js json.T) *T {
	a := js.Ra()
	return &T{
		a[0].Rb(),
		a[1].Rs(),
		a[2].Rs(),
	}
}
