// Copyright 20-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Fleas model data base.

#ifndef DB_FLEAS_FMODELS_H
  #define DB_FLEAS_FMODELS_H

#include "dmc/async.h"
#include "data/flea/Fmodel.h"

/// Initializes data base.
void fmodels_init (void);

/// Returns Arr[FleasEval] pool of best fleas. If model is not found, returns an
/// empty array.
Arr *fmodels_read_pool (Fmodel *model);

/// Adds pool of best fleas. If model is not found, function do nothing.
void fmodels_write_pool (
  Fmodel *model,
  Arr *pool // Arr[FleasEval]
);

/// Returns Arr[FleasEval] historic of best fleas. If model is not found,
/// returns an empty array.
Arr *fmodels_read_bests (Fmodel *model);

/// Adds historic of best fleas. If model is not found, function do nothing.
void fmodels_write_bests (
  Fmodel *model,
  Arr *bests // Arr[FleasEval]
);

/// Returns Arr[Arr[FleasEval]] 10 last ranking (Array 10 (days) of 40 (fleas)).
/// If model is not found, returns an empty array.
Arr *fmodels_read_ranking (Fmodel *model);

/// Adds historic of best fleas. If model is not found, function do nothing.
void fmodels_write_ranking (
  Fmodel *model,
  Arr *ranking // Arr[Arr[FleasEval]] (Array 10 (days) of 40 (fleas)).
);

#endif
