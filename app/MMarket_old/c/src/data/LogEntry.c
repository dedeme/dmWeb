// Copyright 15-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/LogEntry.h"

/* .
# Logs entry
LogEntry: serial
  # 1 if 'this' is a error entry.
  is_error: int
  # Time of message. Its format is 'dd/mm/yyyy(hh:mm:ss)'.
  time: char *
  # Log message.
  msg: char *
*/

/*--*/

struct LogEntry_LogEntry {
  int is_error;
  char *time;
  char *msg;
};

LogEntry *logEntry_new (int is_error, char *time, char *msg) {
  LogEntry *this = MALLOC(LogEntry);
  this->is_error = is_error;
  this->time = time;
  this->msg = msg;
  return this;
}

int logEntry_is_error (LogEntry *this) {
  return this->is_error;
}

char *logEntry_time (LogEntry *this) {
  return this->time;
}

char *logEntry_msg (LogEntry *this) {
  return this->msg;
}

Js *logEntry_to_js (LogEntry *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_wi((int)this->is_error));
  arr_push(js, js_ws(this->time));
  arr_push(js, js_ws(this->msg));
  return js_wa(js);
}

LogEntry *logEntry_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  LogEntry *this = MALLOC(LogEntry);
  this->is_error = js_ri(*p++);
  this->time = js_rs(*p++);
  this->msg = js_rs(*p++);
  return this;
}

/*--*/
