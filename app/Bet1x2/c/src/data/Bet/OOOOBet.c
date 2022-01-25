// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Bet/OOOOBet.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oOOOBet_OOOOBet {
  OOOBet *value;
};

OOOOBet *oOOOBet_mk_none() {
  return (OOOOBet *)opt_mk_none();
}

OOOOBet *oOOOBet_mk_some(OOOBet *value) {
  return (OOOOBet *)opt_mk_some(value);
}

int oOOOBet_none(OOOOBet *opt) {
  return !opt->value;
}

OOOBet *oOOOBet_some(OOOOBet *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

OOOBet *oOOOBet_esome (OOOOBet *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

OOOBet *oOOOBet_osome (OOOOBet *opt, OOOBet *value) {
  return opt->value ? opt->value : value;
}

OOOBet *oOOOBet_nsome (OOOOBet *opt) {
  return opt->value;
}

char *oOOOBet_to_js (OOOOBet *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))oOOBet_to_js);
}

OOOOBet *oOOOBet_from_js (char *js) {
  return (OOOOBet *)opt_from_js(js, (void *(*)(char *))oOOBet_from_js);
}

//--// Not remove

