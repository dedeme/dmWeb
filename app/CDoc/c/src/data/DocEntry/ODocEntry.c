// Copyright 08-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/DocEntry/ODocEntry.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"

struct oDocEntry_ODocEntry {
  DocEntry *value;
};

ODocEntry *oDocEntry_mk_none() {
  return (ODocEntry *)opt_mk_none();
}

ODocEntry *oDocEntry_mk_some(DocEntry *value) {
  return (ODocEntry *)opt_mk_some(value);
}

int oDocEntry_none(ODocEntry *opt) {
  return !opt->value;
}

DocEntry *oDocEntry_some(ODocEntry *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

DocEntry *oDocEntry_esome (ODocEntry *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

DocEntry *oDocEntry_osome (ODocEntry *opt, DocEntry *value) {
  return opt->value ? opt->value : value;
}

DocEntry *oDocEntry_nsome (ODocEntry *opt) {
  return opt->value;
}

char *oDocEntry_to_js (ODocEntry *opt, char *(*to)(DocEntry *e)) {
  return opt->value ? to(opt->value) : "null";
}

ODocEntry *oDocEntry_from_js (char *js, DocEntry *(*from)(char *jse)) {
  return strcmp(js, "null") ? oDocEntry_mk_some(from(js)) : oDocEntry_mk_none();
}

//--// Not remove

