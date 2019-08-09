// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Base for Approx models.

#ifndef DATA_DFLEAS_APPROX_APPROXBASE_H
  #define DATA_DFLEAS_APPROX_APPROXBASE_H

#include "dmc/async.h"
#include "data/Model.h"
#include "dmc/Darr.h"

///
typedef struct approxBase_ApproxBase ApproxBase;

struct approxBase_ApproxBase {
  int to_sell;
  double ref;
};

///
ApproxBase *approxBase_new(int to_sell, double ref);

/// Returns Arr[ApproxBase]
Arr *approxBase_cos (int qnicks, QmatrixValues *closes, double start_to_sell);

///
Order *approxBase_order (
  ApproxBase *this, double q,
  double start_to_buy, double step_to_buy,
  double start_to_sell, double step_to_sell
);

#endif
