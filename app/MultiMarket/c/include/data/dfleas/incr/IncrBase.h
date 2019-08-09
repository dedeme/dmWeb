// Copyright 01-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Base for Incr models.

#ifndef DATA_DFLEAS_INCR_INCRBASE_H
  #define DATA_DFLEAS_INCR_INCRBASE_H

#include "dmc/async.h"
#include "data/Model.h"
#include "dmc/Darr.h"

///
typedef struct incrBase_IncrBase IncrBase;

///
struct incrBase_IncrBase {
  int to_sell;
  QmatrixValues *closes;
  double ref;
  int con; // Company number
  int closes_ix; // Index (day) of Qmatrix
  int days_ix;
};

///
IncrBase *incrBase_new(
  int to_sell, QmatrixValues *closes, double ref, int con
);

/// Returns Arr[IncrBase]
Arr *incrBase_cos (int qnicks, QmatrixValues *closes, double strip_to_sell);

///
Order *incrBase_order (
  IncrBase *this, double q, int days, double strip_to_buy, double strip_to_sell
);

///
double incrBase_ref (
  IncrBase *this, double strip_to_buy, double strip_to_sell
);

#endif
