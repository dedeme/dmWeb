// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Bet/OABet.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oABet_OABet {
  ABet *value;
};

OABet *oABet_mk_none() {
  return (OABet *)opt_mk_none();
}

OABet *oABet_mk_some(ABet *value) {
  return (OABet *)opt_mk_some(value);
}

int oABet_none(OABet *opt) {
  return !opt->value;
}

ABet *oABet_some(OABet *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

ABet *oABet_esome (OABet *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

ABet *oABet_osome (OABet *opt, ABet *value) {
  return opt->value ? opt->value : value;
}

ABet *oABet_nsome (OABet *opt) {
  return opt->value;
}

char *oABet_to_js (OABet *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))aBet_to_js);
}

OABet *oABet_from_js (char *js) {
  return (OABet *)opt_from_js(js, (void *(*)(char *))aBet_from_js);
}

//--// Not remove

