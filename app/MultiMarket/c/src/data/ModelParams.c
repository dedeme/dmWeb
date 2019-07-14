// Copyright 24-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/ModelParams.h"

/* .
ModelParams
  model: Model
  params: Darr
*/

/*--*/

struct ModelParams_ModelParams {
  Model *model;
  Darr *params;
};

ModelParams *modelParams_new (Model *model, Darr *params) {
  ModelParams *this = MALLOC(ModelParams);
  this->model = model;
  this->params = params;
  return this;
}

Model *modelParams_model (ModelParams *this) {
  return this->model;
}

Darr *modelParams_params (ModelParams *this) {
  return this->params;
}

/*--*/

int modelParams_eq (ModelParams *p1, ModelParams *p2) {
  return str_eq(model_name(p1->model), model_name(p2->model)) &&
    darr_eq(p1->params, p2->params, 0.0000001);
}
