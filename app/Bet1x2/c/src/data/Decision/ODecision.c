// Copyright 28-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Decision/ODecision.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oDecision_ODecision {
  Decision *value;
};

ODecision *oDecision_mk_none() {
  return (ODecision *)opt_mk_none();
}

ODecision *oDecision_mk_some(Decision *value) {
  return (ODecision *)opt_mk_some(value);
}

int oDecision_none(ODecision *opt) {
  return !opt->value;
}

Decision *oDecision_some(ODecision *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

Decision *oDecision_esome (ODecision *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

Decision *oDecision_osome (ODecision *opt, Decision *value) {
  return opt->value ? opt->value : value;
}

Decision *oDecision_nsome (ODecision *opt) {
  return opt->value;
}

char *oDecision_to_js (ODecision *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))decision_to_js);
}

ODecision *oDecision_from_js (char *js) {
  return (ODecision *)opt_from_js(js, (void *(*)(char *))decision_from_js);
}

//--// Not remove

