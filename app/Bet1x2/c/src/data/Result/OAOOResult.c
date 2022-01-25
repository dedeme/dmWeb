// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Result/OAOOResult.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oAOOResult_OAOOResult {
  AOOResult *value;
};

OAOOResult *oAOOResult_mk_none() {
  return (OAOOResult *)opt_mk_none();
}

OAOOResult *oAOOResult_mk_some(AOOResult *value) {
  return (OAOOResult *)opt_mk_some(value);
}

int oAOOResult_none(OAOOResult *opt) {
  return !opt->value;
}

AOOResult *oAOOResult_some(OAOOResult *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

AOOResult *oAOOResult_esome (OAOOResult *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

AOOResult *oAOOResult_osome (OAOOResult *opt, AOOResult *value) {
  return opt->value ? opt->value : value;
}

AOOResult *oAOOResult_nsome (OAOOResult *opt) {
  return opt->value;
}

char *oAOOResult_to_js (OAOOResult *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))aOOResult_to_js);
}

OAOOResult *oAOOResult_from_js (char *js) {
  return (OAOOResult *)opt_from_js(js, (void *(*)(char *))aOOResult_from_js);
}

//--// Not remove

