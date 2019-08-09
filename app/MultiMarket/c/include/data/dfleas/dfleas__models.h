// Copyright 12-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Fleas model generator.

#ifndef DATA_DFLEAS_FLEAS__MODELS_H
  #define DATA_DFLEAS_FLEAS__MODELS_H

#include "dmc/async.h"
#include "data/ModelParams.h"

///
void dfleas__models_init (void);

/// Arr[Model]
Arr *dfleas__models (void);

/// Arr[char]
Arr *dfleas__models_names (void);

/// Returns Opt[Model] The model called 'name'
Opt *dfleas__models_get (char *name);

/// Returns Model-Parameters used by default in program initialization.
ModelParams *dfleas__models_default (void);

#endif
