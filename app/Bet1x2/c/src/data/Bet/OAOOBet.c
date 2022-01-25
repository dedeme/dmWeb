// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Bet/OAOOBet.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oAOOBet_OAOOBet {
  AOOBet *value;
};

OAOOBet *oAOOBet_mk_none() {
  return (OAOOBet *)opt_mk_none();
}

OAOOBet *oAOOBet_mk_some(AOOBet *value) {
  return (OAOOBet *)opt_mk_some(value);
}

int oAOOBet_none(OAOOBet *opt) {
  return !opt->value;
}

AOOBet *oAOOBet_some(OAOOBet *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

AOOBet *oAOOBet_esome (OAOOBet *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

AOOBet *oAOOBet_osome (OAOOBet *opt, AOOBet *value) {
  return opt->value ? opt->value : value;
}

AOOBet *oAOOBet_nsome (OAOOBet *opt) {
  return opt->value;
}

char *oAOOBet_to_js (OAOOBet *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))aOOBet_to_js);
}

OAOOBet *oAOOBet_from_js (char *js) {
  return (OAOOBet *)opt_from_js(js, (void *(*)(char *))aOOBet_from_js);
}

//--// Not remove

