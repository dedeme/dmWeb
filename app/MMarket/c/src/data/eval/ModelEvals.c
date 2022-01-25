// Copyright 17-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/eval/ModelEvals.h"
#include "dmc/js.h"
#include "dmc/DEFS.h"

ModelEvals *modelEvals_new (char *date, AModelEval *evals) {
  ModelEvals *this = MALLOC(ModelEvals);
  this->date = date;
  this->evals = evals;
  return this;
}

char *modelEvals_to_js(ModelEvals *this) {
  return js_wa(achar_new_from(
    js_ws(this->date),
    aModelEval_to_js(this->evals),
    NULL
  ));
}

ModelEvals *modelEvals_from_js(char *js) {
  Achar *a = js_ra(js);
  return modelEvals_new(
    js_rs(achar_get(a, 0)),
    aModelEval_from_js(achar_get(a, 1))
  );
}
