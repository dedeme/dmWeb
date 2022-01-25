// Copyright 25-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Doc/ODoc.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

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

char *oDoc_to_js (ODoc *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))doc_to_js);
}

ODoc *oDoc_from_js (char *js) {
  return (ODoc *)opt_from_js(js, (void *(*)(char *))doc_from_js);
}

//--// Not remove

