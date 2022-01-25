// Copyright 22-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Order/OOrder.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oOrder_OOrder {
  Order *value;
};

OOrder *oOrder_mk_none() {
  return (OOrder *)opt_mk_none();
}

OOrder *oOrder_mk_some(Order *value) {
  return (OOrder *)opt_mk_some(value);
}

int oOrder_none(OOrder *opt) {
  return !opt->value;
}

Order *oOrder_some(OOrder *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

Order *oOrder_esome (OOrder *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

Order *oOrder_osome (OOrder *opt, Order *value) {
  return opt->value ? opt->value : value;
}

Order *oOrder_nsome (OOrder *opt) {
  return opt->value;
}

char *oOrder_to_js (OOrder *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))order_to_js);
}

OOrder *oOrder_from_js (char *js) {
  return (OOrder *)opt_from_js(js, (void *(*)(char *))order_from_js);
}

//--// Not remove

