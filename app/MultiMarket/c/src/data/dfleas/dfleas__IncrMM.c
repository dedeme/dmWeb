// Copyright 15-Dec-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/dfleas/dfleas__IncrMM.h"

#include "data/Model.h"
#include "data/dfleas/incrMM/incrMM__3.h"

// Returns Arr[Model]
Arr *dfleas__IncrMM_models (void) {
  //Arr[Model]
  Arr *mds = arr_new();
  arr_push(mds, incrMM__3());
  return mds;
}
