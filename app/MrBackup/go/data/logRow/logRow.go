// Copyright 05-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Log row.
package logRow

import (
	"github.com/dedeme/ktlib/time"
	"github.com/dedeme/ktlib/js"
	"runtime/debug"
	"strings"
)

type T struct {
	error bool
	time  string
	msg   string
}

// Auxiliar function
func now() string {
	return time.Fmt("%D/%M/%Y(%t)", time.Now())
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

// Creates an error message.
//    msg: Message to show.
func Error(msg string) *T {
	return &T{true, now(), msg}
}

func (r *T) ToJs() string {
	return js.Wa([]string{
		js.Wb(r.error),
		js.Ws(r.time),
		js.Ws(r.msg),
	})
}

func FromJs(j string) *T {
	a := js.Ra(j)
	return &T{
		js.Rb(a[0]),
		js.Rs(a[1]),
		js.Rs(a[2]),
	}
}
