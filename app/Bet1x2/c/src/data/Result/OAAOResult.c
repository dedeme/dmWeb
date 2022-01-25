// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Result/OAAOResult.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oAAOResult_OAAOResult {
  AAOResult *value;
};

OAAOResult *oAAOResult_mk_none() {
  return (OAAOResult *)opt_mk_none();
}

OAAOResult *oAAOResult_mk_some(AAOResult *value) {
  return (OAAOResult *)opt_mk_some(value);
}

int oAAOResult_none(OAAOResult *opt) {
  return !opt->value;
}

AAOResult *oAAOResult_some(OAAOResult *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

AAOResult *oAAOResult_esome (OAAOResult *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

AAOResult *oAAOResult_osome (OAAOResult *opt, AAOResult *value) {
  return opt->value ? opt->value : value;
}

AAOResult *oAAOResult_nsome (OAAOResult *opt) {
  return opt->value;
}

char *oAAOResult_to_js (OAAOResult *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))aAOResult_to_js);
}

OAAOResult *oAAOResult_from_js (char *js) {
  return (OAAOResult *)opt_from_js(js, (void *(*)(char *))aAOResult_from_js);
}

//--// Not remove

