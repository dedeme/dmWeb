// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Result/OOOResult.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oOOResult_OOOResult {
  OOResult *value;
};

OOOResult *oOOResult_mk_none() {
  return (OOOResult *)opt_mk_none();
}

OOOResult *oOOResult_mk_some(OOResult *value) {
  return (OOOResult *)opt_mk_some(value);
}

int oOOResult_none(OOOResult *opt) {
  return !opt->value;
}

OOResult *oOOResult_some(OOOResult *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

OOResult *oOOResult_esome (OOOResult *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

OOResult *oOOResult_osome (OOOResult *opt, OOResult *value) {
  return opt->value ? opt->value : value;
}

OOResult *oOOResult_nsome (OOOResult *opt) {
  return opt->value;
}

char *oOOResult_to_js (OOOResult *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))oOResult_to_js);
}

OOOResult *oOOResult_from_js (char *js) {
  return (OOOResult *)opt_from_js(js, (void *(*)(char *))oOResult_from_js);
}

//--// Not remove

