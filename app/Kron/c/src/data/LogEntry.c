// Copyright 13-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/LogEntry.h"
#include <stdio.h>
#include <stdlib.h>
#include "dmc/DEFS.h"
#include "dmc/date.h"
#include "dmc/js.h"

LogEntry *logEntry_new (char *type, char *msg) {
  LogEntry *this = MALLOC(LogEntry);
  this->type = type;
  this->time = date_f(date_now(), "%d/%m/%Y(%T)");
  this->msg = msg;
  return this;
}

///
char *logEntry_to_js (LogEntry *e) {
  return js_wa(achar_new_from(
    js_ws(e->type),
    js_ws(e->time),
    js_ws(e->msg),
    NULL
  ));
}

///
LogEntry *logEntry_from_js (char *js) {
  Achar *a = js_ra(js);
  LogEntry *this = MALLOC(LogEntry);
  char **p = a->es;
  this->type = js_rs(*p++);
  this->time = js_rs(*p++);
  this->msg = js_rs(*p++);
  return this;
}
