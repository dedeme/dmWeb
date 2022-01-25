// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Bet/OAABet.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oAABet_OAABet {
  AABet *value;
};

OAABet *oAABet_mk_none() {
  return (OAABet *)opt_mk_none();
}

OAABet *oAABet_mk_some(AABet *value) {
  return (OAABet *)opt_mk_some(value);
}

int oAABet_none(OAABet *opt) {
  return !opt->value;
}

AABet *oAABet_some(OAABet *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

AABet *oAABet_esome (OAABet *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

AABet *oAABet_osome (OAABet *opt, AABet *value) {
  return opt->value ? opt->value : value;
}

AABet *oAABet_nsome (OAABet *opt) {
  return opt->value;
}

char *oAABet_to_js (OAABet *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))aABet_to_js);
}

OAABet *oAABet_from_js (char *js) {
  return (OAABet *)opt_from_js(js, (void *(*)(char *))aABet_from_js);
}

//--// Not remove

