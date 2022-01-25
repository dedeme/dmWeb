// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Result/OOResult.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oOResult_OOResult {
  OResult *value;
};

OOResult *oOResult_mk_none() {
  return (OOResult *)opt_mk_none();
}

OOResult *oOResult_mk_some(OResult *value) {
  return (OOResult *)opt_mk_some(value);
}

int oOResult_none(OOResult *opt) {
  return !opt->value;
}

OResult *oOResult_some(OOResult *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

OResult *oOResult_esome (OOResult *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

OResult *oOResult_osome (OOResult *opt, OResult *value) {
  return opt->value ? opt->value : value;
}

OResult *oOResult_nsome (OOResult *opt) {
  return opt->value;
}

char *oOResult_to_js (OOResult *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))oResult_to_js);
}

OOResult *oOResult_from_js (char *js) {
  return (OOResult *)opt_from_js(js, (void *(*)(char *))oResult_from_js);
}

//--// Not remove

