// Copyright 04-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Row of log
package logRow

import (
  "github.com/dedeme/golib/json"
)

type T struct {
  isError bool
  time string
  msg string
}

// Creates an error message.
//    time: Its format is 'dd/mm/yyyy(hh:mm:ss)'.
//    msg : Error message.
func NewError (time, msg string) T {
  return T{true, time, msg}
}

// Creates an imformation message.
//    time: Its format is 'dd/mm/yyyy(hh:mm:ss)'.
//    msg : Information message.
func NewInfo (time, msg string) T {
  return T{false, time, msg}
}

func (e T) IsError () bool {
  return e.isError
}

func (e T) Time () string {
  return e.time
}

func (e T) Msg () string {
  return e.msg
}

func (e T) ToJs () json.T {
  return json.Wa([]json.T {
    json.Wb(e.isError),
    json.Ws(e.time),
    json.Ws(e.msg),
  })
}

func FromJs (js json.T) T {
  a := js.Ra()
  return T {
    isError: a[0].Rb(),
    time: a[1].Rs(),
    msg: a[2].Rs(),
  }
}
