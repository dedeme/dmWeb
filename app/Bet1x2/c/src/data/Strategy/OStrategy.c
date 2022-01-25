// Copyright 28-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Strategy/OStrategy.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oStrategy_OStrategy {
  Strategy *value;
};

OStrategy *oStrategy_mk_none() {
  return (OStrategy *)opt_mk_none();
}

OStrategy *oStrategy_mk_some(Strategy *value) {
  return (OStrategy *)opt_mk_some(value);
}

int oStrategy_none(OStrategy *opt) {
  return !opt->value;
}

Strategy *oStrategy_some(OStrategy *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

Strategy *oStrategy_esome (OStrategy *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

Strategy *oStrategy_osome (OStrategy *opt, Strategy *value) {
  return opt->value ? opt->value : value;
}

Strategy *oStrategy_nsome (OStrategy *opt) {
  return opt->value;
}

char *oStrategy_to_js (OStrategy *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))strategy_to_js);
}

OStrategy *oStrategy_from_js (char *js) {
  return (OStrategy *)opt_from_js(js, (void *(*)(char *))strategy_from_js);
}

//--// Not remove

