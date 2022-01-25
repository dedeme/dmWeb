// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Result/OResult.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oResult_OResult {
  Result *value;
};

OResult *oResult_mk_none() {
  return (OResult *)opt_mk_none();
}

OResult *oResult_mk_some(Result *value) {
  return (OResult *)opt_mk_some(value);
}

int oResult_none(OResult *opt) {
  return !opt->value;
}

Result *oResult_some(OResult *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

Result *oResult_esome (OResult *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

Result *oResult_osome (OResult *opt, Result *value) {
  return opt->value ? opt->value : value;
}

Result *oResult_nsome (OResult *opt) {
  return opt->value;
}

char *oResult_to_js (OResult *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))result_to_js);
}

OResult *oResult_from_js (char *js) {
  return (OResult *)opt_from_js(js, (void *(*)(char *))result_from_js);
}

//--// Not remove

int oResult_eq (OResult *this, OResult *other) {
  if (oResult_none(this)) return oResult_none(other);
  return !oResult_none(other) &&
    result_eq(oResult_some(this), oResult_some(other));
}
