// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Base for MMWin models.

#ifndef DATA_DFLEAS_MMWIN_MMWINBASE_H
  #define DATA_DFLEAS_MMWIN_MMWINBASE_H

#include "dmc/async.h"
#include "data/Model.h"
#include "dmc/Darr.h"

///
typedef struct mMWinBase_MMWinBase MMWinBase;

struct mMWinBase_MMWinBase {
  int to_sell;
  double ref;
  double mm;
  double ref0; // mm of day of last operation
  double qop; // close of day of last operation
  double qlast; // close of last day
};

///
MMWinBase *mMWinBase_new(int to_sell, double ref, double max);

/// Returns Arr[MMBase]
Arr *mMWinBase_cos (int qnicks, QmatrixValues *closes);

///
Order *mMWinBase_order (
  MMWinBase *this, double q, double step_to_buy, double step_to_sell
);

///
double mMWinBase_ref (MMWinBase *this);

#endif
