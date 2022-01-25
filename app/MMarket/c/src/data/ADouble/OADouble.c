// Copyright 16-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/ADouble/OADouble.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oADouble_OADouble {
  ADouble *value;
};

OADouble *oADouble_mk_none() {
  return (OADouble *)opt_mk_none();
}

OADouble *oADouble_mk_some(ADouble *value) {
  return (OADouble *)opt_mk_some(value);
}

int oADouble_none(OADouble *opt) {
  return !opt->value;
}

ADouble *oADouble_some(OADouble *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

ADouble *oADouble_esome (OADouble *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

ADouble *oADouble_osome (OADouble *opt, ADouble *value) {
  return opt->value ? opt->value : value;
}

ADouble *oADouble_nsome (OADouble *opt) {
  return opt->value;
}

char *oADouble_to_js (OADouble *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))aDouble_to_js);
}

OADouble *oADouble_from_js (char *js) {
  return (OADouble *)opt_from_js(js, (void *(*)(char *))aDouble_from_js);
}

//--// Not remove

