// Copyright 13-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/ModelMxMn.h"

/* .
# Tuple Name-Maximum-Minimum of a parameter.
ModelMxMn: serial
  name: char *
  max: double
  min: double
*/

/*--*/

struct ModelMxMn_ModelMxMn {
  char *name;
  double max;
  double min;
};

ModelMxMn *modelMxMn_new (char *name, double max, double min) {
  ModelMxMn *this = MALLOC(ModelMxMn);
  this->name = name;
  this->max = max;
  this->min = min;
  return this;
}

char *modelMxMn_name (ModelMxMn *this) {
  return this->name;
}

double modelMxMn_max (ModelMxMn *this) {
  return this->max;
}

double modelMxMn_min (ModelMxMn *this) {
  return this->min;
}

Js *modelMxMn_to_js (ModelMxMn *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(this->name));
  arr_push(js, js_wd(this->max));
  arr_push(js, js_wd(this->min));
  return js_wa(js);
}

ModelMxMn *modelMxMn_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  ModelMxMn *this = MALLOC(ModelMxMn);
  this->name = js_rs(*p++);
  this->max = js_rd(*p++);
  this->min = js_rd(*p++);
  return this;
}

/*--*/

// Returns Arr[char] Names of parameters. 'pmxmns' is Arr[ModelMxMn]
Arr *modelMxMn_names (Arr *pmxmns) {
  // Arr[char]
  Arr *r = arr_new();
  EACH(pmxmns, ModelMxMn, e)
    arr_push(r, e->name);
  _EACH
  return r;
}
