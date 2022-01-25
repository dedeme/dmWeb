// Copyright 25-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Exe/OExe.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oExe_OExe {
  Exe *value;
};

OExe *oExe_mk_none() {
  return (OExe *)opt_mk_none();
}

OExe *oExe_mk_some(Exe *value) {
  return (OExe *)opt_mk_some(value);
}

int oExe_none(OExe *opt) {
  return !opt->value;
}

Exe *oExe_some(OExe *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

Exe *oExe_esome (OExe *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

Exe *oExe_osome (OExe *opt, Exe *value) {
  return opt->value ? opt->value : value;
}

Exe *oExe_nsome (OExe *opt) {
  return opt->value;
}

char *oExe_to_js (OExe *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))exe_to_js);
}

OExe *oExe_from_js (char *js) {
  return (OExe *)opt_from_js(js, (void *(*)(char *))exe_from_js);
}

//--// Not remove

