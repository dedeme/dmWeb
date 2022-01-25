// Copyright 25-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Tbet/OTbet.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oTbet_OTbet {
  Tbet *value;
};

OTbet *oTbet_mk_none() {
  return (OTbet *)opt_mk_none();
}

OTbet *oTbet_mk_some(Tbet *value) {
  return (OTbet *)opt_mk_some(value);
}

int oTbet_none(OTbet *opt) {
  return !opt->value;
}

Tbet *oTbet_some(OTbet *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

Tbet *oTbet_esome (OTbet *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

Tbet *oTbet_osome (OTbet *opt, Tbet *value) {
  return opt->value ? opt->value : value;
}

Tbet *oTbet_nsome (OTbet *opt) {
  return opt->value;
}

char *oTbet_to_js (OTbet *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))tbet_to_js);
}

OTbet *oTbet_from_js (char *js) {
  return (OTbet *)opt_from_js(js, (void *(*)(char *))tbet_from_js);
}

//--// Not remove

