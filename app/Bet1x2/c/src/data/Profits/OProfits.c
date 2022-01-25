// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Profits/OProfits.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oProfits_OProfits {
  Profits *value;
};

OProfits *oProfits_mk_none() {
  return (OProfits *)opt_mk_none();
}

OProfits *oProfits_mk_some(Profits *value) {
  return (OProfits *)opt_mk_some(value);
}

int oProfits_none(OProfits *opt) {
  return !opt->value;
}

Profits *oProfits_some(OProfits *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

Profits *oProfits_esome (OProfits *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

Profits *oProfits_osome (OProfits *opt, Profits *value) {
  return opt->value ? opt->value : value;
}

Profits *oProfits_nsome (OProfits *opt) {
  return opt->value;
}

char *oProfits_to_js (OProfits *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))profits_to_js);
}

OProfits *oProfits_from_js (char *js) {
  return (OProfits *)opt_from_js(js, (void *(*)(char *))profits_from_js);
}

//--// Not remove

