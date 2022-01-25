// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Result/OOAResult.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oOAResult_OOAResult {
  OAResult *value;
};

OOAResult *oOAResult_mk_none() {
  return (OOAResult *)opt_mk_none();
}

OOAResult *oOAResult_mk_some(OAResult *value) {
  return (OOAResult *)opt_mk_some(value);
}

int oOAResult_none(OOAResult *opt) {
  return !opt->value;
}

OAResult *oOAResult_some(OOAResult *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

OAResult *oOAResult_esome (OOAResult *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

OAResult *oOAResult_osome (OOAResult *opt, OAResult *value) {
  return opt->value ? opt->value : value;
}

OAResult *oOAResult_nsome (OOAResult *opt) {
  return opt->value;
}

char *oOAResult_to_js (OOAResult *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))oAResult_to_js);
}

OOAResult *oOAResult_from_js (char *js) {
  return (OOAResult *)opt_from_js(js, (void *(*)(char *))oAResult_from_js);
}

//--// Not remove

