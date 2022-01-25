// Copyright 28-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Decision/OOODecision.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oOODecision_OOODecision {
  OODecision *value;
};

OOODecision *oOODecision_mk_none() {
  return (OOODecision *)opt_mk_none();
}

OOODecision *oOODecision_mk_some(OODecision *value) {
  return (OOODecision *)opt_mk_some(value);
}

int oOODecision_none(OOODecision *opt) {
  return !opt->value;
}

OODecision *oOODecision_some(OOODecision *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

OODecision *oOODecision_esome (OOODecision *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

OODecision *oOODecision_osome (OOODecision *opt, OODecision *value) {
  return opt->value ? opt->value : value;
}

OODecision *oOODecision_nsome (OOODecision *opt) {
  return opt->value;
}

char *oOODecision_to_js (OOODecision *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))oODecision_to_js);
}

OOODecision *oOODecision_from_js (char *js) {
  return (OOODecision *)opt_from_js(js, (void *(*)(char *))oODecision_from_js);
}

//--// Not remove

