// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Models list.

#ifndef DATA_FLEA_FMODELS_H
  #define DATA_FLEA_FMODELS_H

#include "dmc/async.h"

/// Returns Arr[Model]. The model list.
Arr *fmodels_list (void);

/// Returns Opt[Model]
Opt *fmodels_get (char *model_id);

#endif
