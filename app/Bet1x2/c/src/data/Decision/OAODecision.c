// Copyright 28-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Decision/OAODecision.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oAODecision_OAODecision {
  AODecision *value;
};

OAODecision *oAODecision_mk_none() {
  return (OAODecision *)opt_mk_none();
}

OAODecision *oAODecision_mk_some(AODecision *value) {
  return (OAODecision *)opt_mk_some(value);
}

int oAODecision_none(OAODecision *opt) {
  return !opt->value;
}

AODecision *oAODecision_some(OAODecision *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

AODecision *oAODecision_esome (OAODecision *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

AODecision *oAODecision_osome (OAODecision *opt, AODecision *value) {
  return opt->value ? opt->value : value;
}

AODecision *oAODecision_nsome (OAODecision *opt) {
  return opt->value;
}

char *oAODecision_to_js (OAODecision *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))aODecision_to_js);
}

OAODecision *oAODecision_from_js (char *js) {
  return (OAODecision *)opt_from_js(js, (void *(*)(char *))aODecision_from_js);
}

//--// Not remove

