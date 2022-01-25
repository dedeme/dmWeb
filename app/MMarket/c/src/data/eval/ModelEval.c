// Copyright 17-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/eval/ModelEval.h"
#include "dmc/DEFS.h"
#include "dmc/js.h"

ModelEval *modelEval_new (
  ADouble *params, int weeks,
  double hvalue, double hsales, double value, double sales
) {
  ModelEval *this = MALLOC(ModelEval);
  this->params = params;
  this->weeks = weeks;
  this->hvalue = hvalue;
  this->hsales = hsales;
  this->value = value;
  this->sales = sales;
  return this;
}

char *modelEval_to_js(ModelEval *this) {
  return js_wa(achar_new_from(
    aDouble_to_js(this->params),
    js_wi(this->weeks),
    js_wd(this->hvalue),
    js_wd(this->hsales),
    js_wd(this->value),
    js_wd(this->sales),
    NULL
  ));
}

ModelEval *modelEval_from_js(char *js) {
  Achar *a = js_ra(js);
  return modelEval_new(
    aDouble_from_js(achar_get(a, 0)),
    js_ri(achar_get(a, 1)),
    js_rd(achar_get(a, 2)),
    js_rd(achar_get(a, 3)),
    js_rd(achar_get(a, 4)),
    js_rd(achar_get(a, 5))
  );
}
