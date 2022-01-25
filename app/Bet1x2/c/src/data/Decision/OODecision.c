// Copyright 28-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Decision/OODecision.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oODecision_OODecision {
  ODecision *value;
};

OODecision *oODecision_mk_none() {
  return (OODecision *)opt_mk_none();
}

OODecision *oODecision_mk_some(ODecision *value) {
  return (OODecision *)opt_mk_some(value);
}

int oODecision_none(OODecision *opt) {
  return !opt->value;
}

ODecision *oODecision_some(OODecision *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

ODecision *oODecision_esome (OODecision *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

ODecision *oODecision_osome (OODecision *opt, ODecision *value) {
  return opt->value ? opt->value : value;
}

ODecision *oODecision_nsome (OODecision *opt) {
  return opt->value;
}

char *oODecision_to_js (OODecision *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))oDecision_to_js);
}

OODecision *oODecision_from_js (char *js) {
  return (OODecision *)opt_from_js(js, (void *(*)(char *))oDecision_from_js);
}

//--// Not remove

