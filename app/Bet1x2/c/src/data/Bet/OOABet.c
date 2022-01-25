// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Bet/OOABet.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oOABet_OOABet {
  OABet *value;
};

OOABet *oOABet_mk_none() {
  return (OOABet *)opt_mk_none();
}

OOABet *oOABet_mk_some(OABet *value) {
  return (OOABet *)opt_mk_some(value);
}

int oOABet_none(OOABet *opt) {
  return !opt->value;
}

OABet *oOABet_some(OOABet *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

OABet *oOABet_esome (OOABet *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

OABet *oOABet_osome (OOABet *opt, OABet *value) {
  return opt->value ? opt->value : value;
}

OABet *oOABet_nsome (OOABet *opt) {
  return opt->value;
}

char *oOABet_to_js (OOABet *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))oABet_to_js);
}

OOABet *oOABet_from_js (char *js) {
  return (OOABet *)opt_from_js(js, (void *(*)(char *))oABet_from_js);
}

//--// Not remove

