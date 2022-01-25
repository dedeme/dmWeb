// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Bet/OOOBet.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oOOBet_OOOBet {
  OOBet *value;
};

OOOBet *oOOBet_mk_none() {
  return (OOOBet *)opt_mk_none();
}

OOOBet *oOOBet_mk_some(OOBet *value) {
  return (OOOBet *)opt_mk_some(value);
}

int oOOBet_none(OOOBet *opt) {
  return !opt->value;
}

OOBet *oOOBet_some(OOOBet *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

OOBet *oOOBet_esome (OOOBet *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

OOBet *oOOBet_osome (OOOBet *opt, OOBet *value) {
  return opt->value ? opt->value : value;
}

OOBet *oOOBet_nsome (OOOBet *opt) {
  return opt->value;
}

char *oOOBet_to_js (OOOBet *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))oOBet_to_js);
}

OOOBet *oOOBet_from_js (char *js) {
  return (OOOBet *)opt_from_js(js, (void *(*)(char *))oOBet_from_js);
}

//--// Not remove

