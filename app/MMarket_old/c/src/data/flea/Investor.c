// Copyright 22-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/flea/Investor.h"
#include "data/flea/fmodels.h"

/* .
# Record of Model and FleasEval.
Investor:
  model: Fmodel
  eflea: FleasEval
*/

/*--*/

struct Investor_Investor {
  Fmodel *model;
  FleasEval *eflea;
};

Investor *investor_new (Fmodel *model, FleasEval *eflea) {
  Investor *this = MALLOC(Investor);
  this->model = model;
  this->eflea = eflea;
  return this;
}

Fmodel *investor_model (Investor *this) {
  return this->model;
}

FleasEval *investor_eflea (Investor *this) {
  return this->eflea;
}

/*--*/

Js *investor_to_js (Investor *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(fmodel_id(this->model)));
  arr_push(js, fleasEval_to_js(this->eflea));
  return js_wa(js);
}

// Returns Opt[Investor]
Opt *investor_from_js_opt (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  Investor *this = MALLOC(Investor);
  char *model_id = js_rs(*p++);
  this->eflea = fleasEval_from_js(*p++);

  Fmodel *model = opt_nget(fmodels_get(model_id));
  if (!model) return opt_empty();

  this->model = model;
  return opt_new(this);
}

