// Copyright 29-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/OrderNL/OOrderNL.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oOrderNL_OOrderNL {
  OrderNL *value;
};

OOrderNL *oOrderNL_mk_none() {
  return (OOrderNL *)opt_mk_none();
}

OOrderNL *oOrderNL_mk_some(OrderNL *value) {
  return (OOrderNL *)opt_mk_some(value);
}

int oOrderNL_none(OOrderNL *opt) {
  return !opt->value;
}

OrderNL *oOrderNL_some(OOrderNL *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

OrderNL *oOrderNL_esome (OOrderNL *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

OrderNL *oOrderNL_osome (OOrderNL *opt, OrderNL *value) {
  return opt->value ? opt->value : value;
}

OrderNL *oOrderNL_nsome (OOrderNL *opt) {
  return opt->value;
}

char *oOrderNL_to_js (OOrderNL *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))orderNL_to_js);
}

OOrderNL *oOrderNL_from_js (char *js) {
  return (OOrderNL *)opt_from_js(js, (void *(*)(char *))orderNL_from_js);
}

//--// Not remove

