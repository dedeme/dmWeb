// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Base for MMBack models.

#ifndef DATA_DFLEAS_MMBACK_MMBACKBASE_H
  #define DATA_DFLEAS_MMBACK_MMBACKBASE_H

#include "dmc/async.h"
#include "data/Model.h"
#include "dmc/Darr.h"

///
typedef struct mMBackBase_MMBackBase MMBackBase;

struct mMBackBase_MMBackBase {
  int to_sell;
  double ref;
  double mm;
  double pmm1;
  double pmm2;
  double q;
};

///
MMBackBase *mMBackBase_new(int to_sell, double ref, double q);

/// Returns Arr[MMBase]
Arr *mMBackBase_cos (int qnicks, QmatrixValues *closes, double strip_to_sell);

///
Order *mMBackBase_order (
  MMBackBase *this, double q,
  double strip_to_buy, double step_to_buy,
  double strip_to_sell, double step_to_sell
);

#endif
