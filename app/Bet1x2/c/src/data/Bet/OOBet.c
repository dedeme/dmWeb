// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Bet/OOBet.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oOBet_OOBet {
  OBet *value;
};

OOBet *oOBet_mk_none() {
  return (OOBet *)opt_mk_none();
}

OOBet *oOBet_mk_some(OBet *value) {
  return (OOBet *)opt_mk_some(value);
}

int oOBet_none(OOBet *opt) {
  return !opt->value;
}

OBet *oOBet_some(OOBet *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

OBet *oOBet_esome (OOBet *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

OBet *oOBet_osome (OOBet *opt, OBet *value) {
  return opt->value ? opt->value : value;
}

OBet *oOBet_nsome (OOBet *opt) {
  return opt->value;
}

char *oOBet_to_js (OOBet *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))oBet_to_js);
}

OOBet *oOBet_from_js (char *js) {
  return (OOBet *)opt_from_js(js, (void *(*)(char *))oBet_from_js);
}

//--// Not remove

