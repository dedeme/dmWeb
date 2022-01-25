// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Bet/OAAOBet.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oAAOBet_OAAOBet {
  AAOBet *value;
};

OAAOBet *oAAOBet_mk_none() {
  return (OAAOBet *)opt_mk_none();
}

OAAOBet *oAAOBet_mk_some(AAOBet *value) {
  return (OAAOBet *)opt_mk_some(value);
}

int oAAOBet_none(OAAOBet *opt) {
  return !opt->value;
}

AAOBet *oAAOBet_some(OAAOBet *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

AAOBet *oAAOBet_esome (OAAOBet *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

AAOBet *oAAOBet_osome (OAAOBet *opt, AAOBet *value) {
  return opt->value ? opt->value : value;
}

AAOBet *oAAOBet_nsome (OAAOBet *opt) {
  return opt->value;
}

char *oAAOBet_to_js (OAAOBet *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))aAOBet_to_js);
}

OAAOBet *oAAOBet_from_js (char *js) {
  return (OAAOBet *)opt_from_js(js, (void *(*)(char *))aAOBet_from_js);
}

//--// Not remove

