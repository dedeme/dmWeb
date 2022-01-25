// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Result/OAResult.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oAResult_OAResult {
  AResult *value;
};

OAResult *oAResult_mk_none() {
  return (OAResult *)opt_mk_none();
}

OAResult *oAResult_mk_some(AResult *value) {
  return (OAResult *)opt_mk_some(value);
}

int oAResult_none(OAResult *opt) {
  return !opt->value;
}

AResult *oAResult_some(OAResult *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

AResult *oAResult_esome (OAResult *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

AResult *oAResult_osome (OAResult *opt, AResult *value) {
  return opt->value ? opt->value : value;
}

AResult *oAResult_nsome (OAResult *opt) {
  return opt->value;
}

char *oAResult_to_js (OAResult *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))aResult_to_js);
}

OAResult *oAResult_from_js (char *js) {
  return (OAResult *)opt_from_js(js, (void *(*)(char *))aResult_from_js);
}

//--// Not remove

