// Copyright 15-Dec-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_DFLEAS_INCRMM_INCRMMBASE_H
  #define DATA_DFLEAS_INCRMM_INCRMMBASE_H

#include "dmc/async.h"
#include "data/Model.h"
#include "dmc/Darr.h"

///
typedef struct incrMMBase_IncrMMBase IncrMMBase;

///
struct incrMMBase_IncrMMBase {
  int to_sell;
  QmatrixValues *closes;
  double ref; // ref = closes[days_ix + day_mm_ix][con] +- strip
  int con; // Company number
  int closes_ix; // Index (day) of Qmatrix
  int days_ix;
  int day_mm_ix;
};

///
IncrMMBase *incrMMBase_new(
  int to_sell, QmatrixValues *closes, int con
);

/// Returns Arr[IncrBase]
Arr *incrMMBase_cos (int qnicks, QmatrixValues *closes, double strip_to_sell);

///
Order *incrMMBase_order (
  IncrMMBase *this, double q, int days,
  double strip_to_buy, double strip_to_sell
);

///
double incrMMBase_ref (
  IncrMMBase *this, double strip_to_buy, double strip_to_sell
);

#endif
