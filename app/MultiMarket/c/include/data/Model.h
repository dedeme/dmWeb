// Copyright 11-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Interface for flea models.

#ifndef DATA_MODEL_H
  #define DATA_MODEL_H

#include "dmc/async.h"
#include "dmc/Darr.h"
#include "Flea.h"
#include "Order.h"
#include "Rs.h"
#include "Qmatrix.h"
#include "NickSets.h"
#include "ModelMxMn.h"

/// Function which returns parameters of 'f'
typedef Darr *(*Fparams)(Flea *f);

/// Function which creates companies data.
///   params: Parameters (returned by 'model_params')
///   qnicks: Number of nicks-companies (columns of closes)
///   closes: Array of HISTORIC_QUOTES QmatrixValues (on for each date).
///   return: Arr[Co]. 'Co' changes for every model.
typedef Arr *(*Fcos)(Darr *params, int qnicks, QmatrixValues *closes);

/// Function which returns a market order.
///   params: Parameters (returned by model_params)
///   co: Company data. It is one of array elements returned by 'model_cos'
///       and 'Fcos'. It will be modified.
///   q: Close quote.
typedef Order *(*Forder)(Darr *params, void *co, double q);

/// Function wich returns actual current reference for calculating orders.
///   params: Parameters (returned by model_params)
///   co: Company data. It is one of elements returned by 'model_cos' and
///       'Fcos'.
typedef double (*Fref)(Darr *params, void *co);

///
///   Arguments:
///     name: char*
///     param_cf: Arr-ModelMxMn
///     param_jss: Js
///     fparams: Fparams
///     fcos: Fcos
///     forder: Forder
///     fref: Fref
typedef struct Model_Model Model;

///
Model *model_new (
  char *name,
  Arr *param_cf,
  Js *param_jss,
  Fparams fparams,
  Fcos fcos,
  Forder forder,
  Fref fref
);

/// Model name.
char *model_name (Model *this);

/// Arr[ModelMxMn] Names of model parameters.
Arr *model_param_cf (Model *this);

/// Template to show params in javascript (prefix-multiplicator-decimals-suffix)
Js *model_param_jss (Model *this);

/// Returns parameters of 'f'.
Darr *model_params(Model *this, Flea *f);

/// Creates companies data.
///   this: Model
///   params: Parameters (returned by 'model_params')
///   qnicks: Number of nicks-companies (columns of closes)
///   closes: Array of HISTORIC_QUOTES QmatrixValues (on for each date).
///   return: Arr[Co]. 'Co' changes for every model.
Arr *model_cos(Model *this, Darr *params, int qnicks, QmatrixValues *closes);

/// Returns a market order.
///   this: Model
///   params: Parameters (returned by model_params)
///   co: Company data. It is one of array elements returned by 'model_cos'
///       and 'Fcos'. It will be modified.
///   q: Close quote.
Order *model_order(Model *this, Darr *params, void *co, double q);

/// Returns actual current reference for calculating orders.
///   this: Model
///   params: Parameters (returned by model_params)
///   co: Company data. It is one of elements returned by 'model_cos' and
///       'Fcos'.
double model_ref(Model *this, Darr *params, void *co);

/// Calculates flea assets
RsAssets *model_assets(
  Model *this, Flea *f, NickSets *sets, Qmatrix *opens, Qmatrix *closes
);

/// Calculates flea profits
RsProfits *model_profits(
  Model *this, Flea *f, NickSets *sets, Qmatrix *opens, Qmatrix *closes
);

/// Calculates data for charts. 'dates' is Arr[char]
RsCharts *model_charts(
  Model *this, Flea *f, Arr *dates, Qmatrix *opens, Qmatrix *closes
);

/// Calculates historic results for one company
RsHistoric *model_historic (
  Model *this, Darr *params, Arr *dates, Darr *opens, Darr *closes
);

#endif
