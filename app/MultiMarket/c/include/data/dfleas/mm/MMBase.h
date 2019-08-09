// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Base for MM models.

#ifndef DATA_DFLEAS_MM_MMBASE_H
  #define DATA_DFLEAS_MM_MMBASE_H

#include "dmc/async.h"
#include "data/Model.h"
#include "dmc/Darr.h"

///
typedef struct mMBase_MMBase MMBase;

struct mMBase_MMBase {
  int to_sell;
  double ref;
  double mm;
};

///
MMBase *mMBase_new(int to_sell, double ref, double q);

/// Returns Arr[MMBase]
Arr *mMBase_cos (int qnicks, QmatrixValues *closes, double strip_to_sell);

///
Order *mMBase_order (
  MMBase *this, double q,
  double strip_to_buy, double step_to_buy,
  double strip_to_sell, double step_to_sell
);

#endif
