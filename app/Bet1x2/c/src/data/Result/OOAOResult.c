// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Result/OOAOResult.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oOAOResult_OOAOResult {
  OAOResult *value;
};

OOAOResult *oOAOResult_mk_none() {
  return (OOAOResult *)opt_mk_none();
}

OOAOResult *oOAOResult_mk_some(OAOResult *value) {
  return (OOAOResult *)opt_mk_some(value);
}

int oOAOResult_none(OOAOResult *opt) {
  return !opt->value;
}

OAOResult *oOAOResult_some(OOAOResult *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

OAOResult *oOAOResult_esome (OOAOResult *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

OAOResult *oOAOResult_osome (OOAOResult *opt, OAOResult *value) {
  return opt->value ? opt->value : value;
}

OAOResult *oOAOResult_nsome (OOAOResult *opt) {
  return opt->value;
}

char *oOAOResult_to_js (OOAOResult *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))oAOResult_to_js);
}

OOAOResult *oOAOResult_from_js (char *js) {
  return (OOAOResult *)opt_from_js(js, (void *(*)(char *))oAOResult_from_js);
}

//--// Not remove

