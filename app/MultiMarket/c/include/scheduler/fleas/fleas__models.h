// Copyright 12-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Fleas model generator.

#ifndef SCHEDULER_FLEAS_FLEAS__MODELS_H
  #define SCHEDULER_FLEAS_FLEAS__MODELS_H

#include "dmc/std.h"

///
void fleas__models_init (void);

/// Arr[Model]
Arr *fleas__models (void);

/// Arr[char]
Arr *fleas__models_names (void);

/// Returns Opt[Model] The model called 'name'
Opt *fleas__models_get (char *name);

#endif
