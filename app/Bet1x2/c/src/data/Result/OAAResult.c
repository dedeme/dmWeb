// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Result/OAAResult.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oAAResult_OAAResult {
  AAResult *value;
};

OAAResult *oAAResult_mk_none() {
  return (OAAResult *)opt_mk_none();
}

OAAResult *oAAResult_mk_some(AAResult *value) {
  return (OAAResult *)opt_mk_some(value);
}

int oAAResult_none(OAAResult *opt) {
  return !opt->value;
}

AAResult *oAAResult_some(OAAResult *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

AAResult *oAAResult_esome (OAAResult *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

AAResult *oAAResult_osome (OAAResult *opt, AAResult *value) {
  return opt->value ? opt->value : value;
}

AAResult *oAAResult_nsome (OAAResult *opt) {
  return opt->value;
}

char *oAAResult_to_js (OAAResult *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))aAResult_to_js);
}

OAAResult *oAAResult_from_js (char *js) {
  return (OAAResult *)opt_from_js(js, (void *(*)(char *))aAResult_from_js);
}

//--// Not remove

