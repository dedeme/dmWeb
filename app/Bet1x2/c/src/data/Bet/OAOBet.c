// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Bet/OAOBet.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oAOBet_OAOBet {
  AOBet *value;
};

OAOBet *oAOBet_mk_none() {
  return (OAOBet *)opt_mk_none();
}

OAOBet *oAOBet_mk_some(AOBet *value) {
  return (OAOBet *)opt_mk_some(value);
}

int oAOBet_none(OAOBet *opt) {
  return !opt->value;
}

AOBet *oAOBet_some(OAOBet *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

AOBet *oAOBet_esome (OAOBet *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

AOBet *oAOBet_osome (OAOBet *opt, AOBet *value) {
  return opt->value ? opt->value : value;
}

AOBet *oAOBet_nsome (OAOBet *opt) {
  return opt->value;
}

char *oAOBet_to_js (OAOBet *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))aOBet_to_js);
}

OAOBet *oAOBet_from_js (char *js) {
  return (OAOBet *)opt_from_js(js, (void *(*)(char *))aOBet_from_js);
}

//--// Not remove

