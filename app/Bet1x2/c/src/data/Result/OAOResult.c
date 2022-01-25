// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Result/OAOResult.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oAOResult_OAOResult {
  AOResult *value;
};

OAOResult *oAOResult_mk_none() {
  return (OAOResult *)opt_mk_none();
}

OAOResult *oAOResult_mk_some(AOResult *value) {
  return (OAOResult *)opt_mk_some(value);
}

int oAOResult_none(OAOResult *opt) {
  return !opt->value;
}

AOResult *oAOResult_some(OAOResult *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

AOResult *oAOResult_esome (OAOResult *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

AOResult *oAOResult_osome (OAOResult *opt, AOResult *value) {
  return opt->value ? opt->value : value;
}

AOResult *oAOResult_nsome (OAOResult *opt) {
  return opt->value;
}

char *oAOResult_to_js (OAOResult *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))aOResult_to_js);
}

OAOResult *oAOResult_from_js (char *js) {
  return (OAOResult *)opt_from_js(js, (void *(*)(char *))aOResult_from_js);
}

//--// Not remove

