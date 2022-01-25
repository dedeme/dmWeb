// Copyright 28-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Decision/OADecision.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oADecision_OADecision {
  ADecision *value;
};

OADecision *oADecision_mk_none() {
  return (OADecision *)opt_mk_none();
}

OADecision *oADecision_mk_some(ADecision *value) {
  return (OADecision *)opt_mk_some(value);
}

int oADecision_none(OADecision *opt) {
  return !opt->value;
}

ADecision *oADecision_some(OADecision *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

ADecision *oADecision_esome (OADecision *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

ADecision *oADecision_osome (OADecision *opt, ADecision *value) {
  return opt->value ? opt->value : value;
}

ADecision *oADecision_nsome (OADecision *opt) {
  return opt->value;
}

char *oADecision_to_js (OADecision *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))aDecision_to_js);
}

OADecision *oADecision_from_js (char *js) {
  return (OADecision *)opt_from_js(js, (void *(*)(char *))aDecision_from_js);
}

//--// Not remove

