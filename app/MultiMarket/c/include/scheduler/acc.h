// Copyright 29-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Accounting.

#ifndef SCHEDULER_ACC_H
  #define SCHEDULER_ACC_H

#include "dmc/async.h"

/// Calculates and annotates daily operations using historic data.
void acc_operations_from_historic (AsyncActor *ac);

/// Calculates and annotates daily operations using historic and daily data.
void acc_operations_from_daily (AsyncActor *ac);

#endif
