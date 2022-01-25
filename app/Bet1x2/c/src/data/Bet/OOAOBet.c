// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Bet/OOAOBet.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oOAOBet_OOAOBet {
  OAOBet *value;
};

OOAOBet *oOAOBet_mk_none() {
  return (OOAOBet *)opt_mk_none();
}

OOAOBet *oOAOBet_mk_some(OAOBet *value) {
  return (OOAOBet *)opt_mk_some(value);
}

int oOAOBet_none(OOAOBet *opt) {
  return !opt->value;
}

OAOBet *oOAOBet_some(OOAOBet *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

OAOBet *oOAOBet_esome (OOAOBet *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

OAOBet *oOAOBet_osome (OOAOBet *opt, OAOBet *value) {
  return opt->value ? opt->value : value;
}

OAOBet *oOAOBet_nsome (OOAOBet *opt) {
  return opt->value;
}

char *oOAOBet_to_js (OOAOBet *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))oAOBet_to_js);
}

OOAOBet *oOAOBet_from_js (char *js) {
  return (OOAOBet *)opt_from_js(js, (void *(*)(char *))oAOBet_from_js);
}

//--// Not remove

