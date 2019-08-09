// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/dfleas/dfleas__MA.h"
#include "data/Model.h"
#include "data/dfleas/ma/ma__3.h"
#include "data/dfleas/ma/ma__2.h"

// Returns Arr[Model]
Arr *dfleas__MA_models (void) {
  //Arr[Model]
  Arr *mds = arr_new();
  arr_push(mds, ma__3());
  arr_push(mds, ma__2());
  return mds;
}

