// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Base for MA models

#ifndef DATA_DFLEAS_MA_MABASE_H
  #define DATA_DFLEAS_MA_MABASE_H

#include "dmc/async.h"
#include "data/Model.h"
#include "dmc/Darr.h"

///
typedef struct mABase_MABase MABase;

struct mABase_MABase {
  int to_sell;
  double sum;
  QmatrixValues *closes;
  int con; // Company number
  int closes_ix; // Index (day) of Qmatrix
  int days_ix;
};

///
MABase *mABase_new(QmatrixValues *closes, int con, int days);

/// Returns Arr[IncrBase]
Arr *mABase_cos (int qnicks, QmatrixValues *closes, int days);

///
Order *mABase_order (
  MABase *this, double q, int days, double strip_to_buy, double strip_to_sell
);

///
double mABase_ref (
  MABase *this, double strip_to_buy, double strip_to_sell
);

#endif
