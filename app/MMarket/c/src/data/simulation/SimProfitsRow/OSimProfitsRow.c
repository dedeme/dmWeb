// Copyright 28-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/simulation/SimProfitsRow/OSimProfitsRow.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oSimProfitsRow_OSimProfitsRow {
  SimProfitsRow *value;
};

OSimProfitsRow *oSimProfitsRow_mk_none() {
  return (OSimProfitsRow *)opt_mk_none();
}

OSimProfitsRow *oSimProfitsRow_mk_some(SimProfitsRow *value) {
  return (OSimProfitsRow *)opt_mk_some(value);
}

int oSimProfitsRow_none(OSimProfitsRow *opt) {
  return !opt->value;
}

SimProfitsRow *oSimProfitsRow_some(OSimProfitsRow *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

SimProfitsRow *oSimProfitsRow_esome (OSimProfitsRow *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

SimProfitsRow *oSimProfitsRow_osome (OSimProfitsRow *opt, SimProfitsRow *value) {
  return opt->value ? opt->value : value;
}

SimProfitsRow *oSimProfitsRow_nsome (OSimProfitsRow *opt) {
  return opt->value;
}

char *oSimProfitsRow_to_js (OSimProfitsRow *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))simProfitsRow_to_js);
}

OSimProfitsRow *oSimProfitsRow_from_js (char *js) {
  return (OSimProfitsRow *)opt_from_js(js, (void *(*)(char *))simProfitsRow_from_js);
}

//--// Not remove

