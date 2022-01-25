// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Bet/OBet.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oBet_OBet {
  Bet *value;
};

OBet *oBet_mk_none() {
  return (OBet *)opt_mk_none();
}

OBet *oBet_mk_some(Bet *value) {
  return (OBet *)opt_mk_some(value);
}

int oBet_none(OBet *opt) {
  return !opt->value;
}

Bet *oBet_some(OBet *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

Bet *oBet_esome (OBet *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

Bet *oBet_osome (OBet *opt, Bet *value) {
  return opt->value ? opt->value : value;
}

Bet *oBet_nsome (OBet *opt) {
  return opt->value;
}

char *oBet_to_js (OBet *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))bet_to_js);
}

OBet *oBet_from_js (char *js) {
  return (OBet *)opt_from_js(js, (void *(*)(char *))bet_from_js);
}

//--// Not remove

