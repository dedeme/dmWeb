// Copyright 16-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Model/OModel.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oModel_OModel {
  Model *value;
};

OModel *oModel_mk_none() {
  return (OModel *)opt_mk_none();
}

OModel *oModel_mk_some(Model *value) {
  return (OModel *)opt_mk_some(value);
}

int oModel_none(OModel *opt) {
  return !opt->value;
}

Model *oModel_some(OModel *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

Model *oModel_esome (OModel *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

Model *oModel_osome (OModel *opt, Model *value) {
  return opt->value ? opt->value : value;
}

Model *oModel_nsome (OModel *opt) {
  return opt->value;
}

char *oModel_to_js (OModel *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))model_to_js);
}

OModel *oModel_from_js (char *js) {
  return (OModel *)opt_from_js(js, (void *(*)(char *))model_from_js);
}

//--// Not remove

