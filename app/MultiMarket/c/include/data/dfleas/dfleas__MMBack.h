// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// MMBack models.<p>
/// Its main difference with MM models is that when last operation is a lost
/// start at the maximum-minimum of two cycles (MM models start at last
/// cycle ever)

#ifndef DATA_DFLEAS_DFLEAS__MMBACK_H
  #define DATA_DFLEAS_DFLEAS__MMBACK_H

#include "dmc/async.h"
#include "data/Model.h"

/// Returns Arr[Model]
Arr *dfleas__MMBack_models (void);

/// Returns default model
Model *dfleas__MMBack_default (void);

#endif
