// Copyright 17-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/eval/ModelEval/OModelEval.h"
#include <string.h>
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "dmc/Opt.h"
#include "dmc/js.h"

struct oModelEval_OModelEval {
  ModelEval *value;
};

OModelEval *oModelEval_mk_none() {
  return (OModelEval *)opt_mk_none();
}

OModelEval *oModelEval_mk_some(ModelEval *value) {
  return (OModelEval *)opt_mk_some(value);
}

int oModelEval_none(OModelEval *opt) {
  return !opt->value;
}

ModelEval *oModelEval_some(OModelEval *opt) {
  if (opt->value) return opt->value;
  FAIL("Option is none");
  return NULL; // Unreachable
}

ModelEval *oModelEval_esome (OModelEval *opt, char *msg) {
  if (opt->value) return opt->value;
  FAIL(msg);
  return NULL; // Unreachable
}

ModelEval *oModelEval_osome (OModelEval *opt, ModelEval *value) {
  return opt->value ? opt->value : value;
}

ModelEval *oModelEval_nsome (OModelEval *opt) {
  return opt->value;
}

char *oModelEval_to_js (OModelEval *opt) {
  return opt_to_js((Opt *)opt, (char *(*)(void *))modelEval_to_js);
}

OModelEval *oModelEval_from_js (char *js) {
  return (OModelEval *)opt_from_js(js, (void *(*)(char *))modelEval_from_js);
}

//--// Not remove

