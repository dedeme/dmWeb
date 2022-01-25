// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Result/OOOOResult.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oOOOResult_OOOOResult {
  OOOResult *value;
};

OOOOResult *oOOOResult_mk_none() {
  return (OOOOResult *)opt_mk_none();
}

OOOOResult *oOOOResult_mk_some(OOOResult *value) {
  return (OOOOResult *)opt_mk_some(value);
}

int oOOOResult_none(OOOOResult *opt) {
  return !opt->value;
}

OOOResult *oOOOResult_some(OOOOResult *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

OOOResult *oOOOResult_esome (OOOOResult *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

OOOResult *oOOOResult_osome (OOOOResult *opt, OOOResult *value) {
  return opt->value ? opt->value : value;
}

OOOResult *oOOOResult_nsome (OOOOResult *opt) {
  return opt->value;
}

char *oOOOResult_to_js (OOOOResult *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))oOOResult_to_js);
}

OOOOResult *oOOOResult_from_js (char *js) {
  return (OOOOResult *)opt_from_js(js, (void *(*)(char *))oOOResult_from_js);
}

//--// Not remove

