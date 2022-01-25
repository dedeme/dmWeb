// Copyright 13-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/LogEntry/OLogEntry.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"

struct oLogEntry_OLogEntry {
  LogEntry *value;
};

OLogEntry *oLogEntry_mk_none() {
  return (OLogEntry *)opt_mk_none();
}

OLogEntry *oLogEntry_mk_some(LogEntry *value) {
  return (OLogEntry *)opt_mk_some(value);
}

int oLogEntry_none(OLogEntry *opt) {
  return !opt->value;
}

LogEntry *oLogEntry_some(OLogEntry *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

LogEntry *oLogEntry_esome (OLogEntry *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

LogEntry *oLogEntry_osome (OLogEntry *opt, LogEntry *value) {
  return opt->value ? opt->value : value;
}

LogEntry *oLogEntry_nsome (OLogEntry *opt) {
  return opt->value;
}

char *oLogEntry_to_js (OLogEntry *opt, char *(*to)(LogEntry *e)) {
  return opt->value ? to(opt->value) : "null";
}

OLogEntry *oLogEntry_from_js (char *js, LogEntry *(*from)(char *jse)) {
  return strcmp(js, "null") ? oLogEntry_mk_some(from(js)) : oLogEntry_mk_none();
}

//--// Not remove

