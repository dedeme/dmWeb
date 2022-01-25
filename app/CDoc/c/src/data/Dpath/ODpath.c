// Copyright 25-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Dpath/ODpath.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oDpath_ODpath {
  Dpath *value;
};

ODpath *oDpath_mk_none() {
  return (ODpath *)opt_mk_none();
}

ODpath *oDpath_mk_some(Dpath *value) {
  return (ODpath *)opt_mk_some(value);
}

int oDpath_none(ODpath *opt) {
  return !opt->value;
}

Dpath *oDpath_some(ODpath *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

Dpath *oDpath_esome (ODpath *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

Dpath *oDpath_osome (ODpath *opt, Dpath *value) {
  return opt->value ? opt->value : value;
}

Dpath *oDpath_nsome (ODpath *opt) {
  return opt->value;
}

char *oDpath_to_js (ODpath *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))dpath_to_js);
}

ODpath *oDpath_from_js (char *js) {
  return (ODpath *)opt_from_js(js, (void *(*)(char *))dpath_from_js);
}

//--// Not remove

