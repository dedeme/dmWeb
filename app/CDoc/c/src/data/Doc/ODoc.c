// Copyright 11-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Doc/ODoc.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"

struct oDoc_ODoc {
  Doc *value;
};

ODoc *oDoc_mk_none() {
  return (ODoc *)opt_mk_none();
}

ODoc *oDoc_mk_some(Doc *value) {
  return (ODoc *)opt_mk_some(value);
}

int oDoc_none(ODoc *opt) {
  return !opt->value;
}

Doc *oDoc_some(ODoc *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

Doc *oDoc_esome (ODoc *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

Doc *oDoc_osome (ODoc *opt, Doc *value) {
  return opt->value ? opt->value : value;
}

Doc *oDoc_nsome (ODoc *opt) {
  return opt->value;
}

char *oDoc_to_js (ODoc *opt, char *(*to)(Doc *e)) {
  return opt->value ? to(opt->value) : "null";
}

ODoc *oDoc_from_js (char *js, Doc *(*from)(char *jse)) {
  return strcmp(js, "null") ? oDoc_mk_some(from(js)) : oDoc_mk_none();
}

//--// Not remove

