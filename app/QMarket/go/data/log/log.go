// Copyright 14-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Log row.
package log

import (
	"github.com/dedeme/golib/date"
	"github.com/dedeme/golib/json"
	"runtime/debug"
	"strings"
)

type rowT struct {
	error bool
	time  string
	msg   string
}

// Log type
type T []*rowT

// Auxiliar function
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
func mkInfo(msg string) *rowT {
	return &rowT{false, now(), msg}
}

// Creates an error message with stack trace.
//    msg: Message to show.
func mkError(msg string) *rowT {
	return &rowT{true, now(), Format(msg)}
}

func (r *rowT) toJs() json.T {
	return json.Wa([]json.T{
		json.Wb(r.error),
		json.Ws(r.time),
		json.Ws(r.msg),
	})
}

func fromJs(js json.T) *rowT {
	a := js.Ra()
	return &rowT{
		a[0].Rb(),
		a[1].Rs(),
		a[2].Rs(),
	}
}

// Returns an empty log
func New() (l T) {
	return
}

// Add an information message to 'l'
func (l T) AddInfo(msg string) T {
	return append(l, mkInfo(msg))
}

// Add an error message to 'l'
func (l T) AddError(msg string) T {
	return append(l, mkError(msg))
}

func (l T) ToJs() json.T {
	var jss []json.T
	for _, row := range l {
		jss = append(jss, row.toJs())
	}
	return json.Wa(jss)
}

func FromJs(js json.T) T {
	var l T
	for _, js := range js.Ra() {
		l = append(l, fromJs(js))
	}
	return l
}
