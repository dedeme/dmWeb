// Copyright 24-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Tuple model-parameters of the model used in accounting.

#ifndef DATA_MODELPARAMS_H
  #define DATA_MODELPARAMS_H

#include "dmc/std.h"
#include "dmc/Darr.h"
#include "Model.h"

/*--*/

///
///   Arguments:
///     model: Model
///     params: Darr
typedef struct ModelParams_ModelParams ModelParams;

///
ModelParams *modelParams_new (Model *model, Darr *params);

///
Model *modelParams_model (ModelParams *this);

///
Darr *modelParams_params (ModelParams *this);

/*--*/

/// Returns 1 if model names are equals and params are equals with a gap of
/// 0.0000001
int modelParams_eq (ModelParams *p1, ModelParams *p2);
#endif
