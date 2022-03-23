// Copyright 16-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Investor model.

#ifndef DATA_MODEL_H
  #define DATA_MODEL_H

#include "dmc/char/Achar.h"
#include "data/ADouble/AADouble.h"
#include "data/Quotes.h"
#include "data/eval/ModelEval/AModelEval.h"
#include "data/simulation/SimProfitsRow/ASimProfitsRow.h"
#include "data/eval/Result.h"
#include "data/Order/AOrder.h"

/// Investor model.
struct model_Model {
  // Short name.
  char *id;
  // Long name.
  char *name;
  // Documentation.
  char *doc;
  // Parameter names.
  Achar *param_names;
  // First parameters to calculate.
  double *param_bases;
  // Increment of parameters to calculate.
  double *param_base_incs;
  // Increment of parameters to calculate its environment.
  double *param_env_incs;
  // Function to calculate operations.
  //    closes: Closes in matrix 'dates x cos' ordered from before to after.
  //    params: Values to calculate.
  //    action: Function called after calculate 'refs'
  //            When closes[i] >= refs[i], position is bought.
  //            When closes[i] < refs[i], position is sold.
  //            Params:
  //              closes: Last closes. One for each company.
  //              refs  : Last references. One for each company.
  void (*calc)(
    AADouble *closes,
    double *params,
    void (*action)(ADouble *closes, ADouble *refs)
  );
};

/// Investor model.
typedef struct model_Model Model;

/// Constructor
///   id             : Short name.
///   name           : Long name.
///   doc            : Documentation.
///   param_name     : Parameter names.
///   param_bases    : First parameters to calculate.
///   param_base_incs: Increment of parameters to calculate.
///   param_env_incs : Increment of parameters to calculate its environment.
///   calc           : Function to calculate operations.
///     closes: Closes in matrix 'dates x cos' ordered from before to after.
///     params: Values to calculate.
///     action: Function called after calculate 'refs'
///             When closes[i] > refs[i], position is bought.
///             When closes[i] < refs[i], position is selled.
///             Params:
///               closes: Last closes. One for each company.
///               refs  : Last references. One for each company.
Model *model_new (
  char *id, char *name, char *doc,
  Achar *param_names,
  double *param_bases, double *param_base_incs, double *param_env_incs,
  void (*calc)(
    AADouble *closes,
    double *params,
    void (*action)(ADouble *closes, ADouble *refs)
  )
);

/// Returns the trading result of a parameter.
Result *model_result (Model *this, Quotes *qs, ADouble *params);

/// Returns the trading result of a parameters group.
Result *model_group_result (Model *this, Quotes *qs, ADouble *params);

/// Reevaluate 'evals' in a new AModelEval, adding a new evaluation.
AModelEval *model_range_new_evaluation (
  Model *this, Quotes *qs, AModelEval *evals
);

/// Reevaluate 'evals' in a new AModelEval, replacing the last evaluation.
AModelEval *model_range_replace_evaluation (
  Model *this, Quotes *qs, AModelEval *evals
);

/// Reevaluate 'profits' in a new AModelEval, adding a new evaluation.
ASimProfitsRow *model_simulation_new(
  Model *this, Quotes *qs, ASimProfitsRow *profits
);

/// Reevaluate 'profits' in a new AModelEval, replacing the last evaluation.
ASimProfitsRow *model_simulation_replace(
  Model *this, Quotes *qs, ASimProfitsRow *profits
);

/// Returns references of one company.
///   this  :
///   co_qs : Quotes of one company.
///   params: Parameters to calculate references.
ADouble *model_refs(Model *this, Quotes *co_qs, ADouble *params);

/// Returns historic assets of one model,
///   this  :
///   qs    : Quotes of all the companies,
///   params: Parameters to calculate references.
ADouble *model_historic(Model *this, Quotes *qs, ADouble *params);

/// Returns orders of one model.
///   this  :
///   qs    : Quotes of all the companies,
///   params: Parameters to calculate references.
AOrder *model_orders(Model *this, Quotes *qs, ADouble *params);

/// Partial serialization.
char *model_to_js (Model *this);

/// Unimplemented.
Model *model_from_js (char *js);

#endif
