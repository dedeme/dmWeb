// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/dfleas/dfleas__GA.h"
#include "data/Model.h"
#include "data/dfleas/ga/ga__3.h"

// Returns Arr[Model]
Arr *dfleas__GA_models (void) {
  //Arr[Model]
  Arr *mds = arr_new();
  arr_push(mds, ga__3());
  return mds;
}

