// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/dfleas/dfleas__Approx.h"
#include "data/Model.h"
#include "data/dfleas/approx/approx__4.h"
#include "data/dfleas/approx/approx__3A.h"
#include "data/dfleas/approx/approx__3B.h"
#include "data/dfleas/approx/approx__2.h"

// Returns Arr[Model]
Arr *dfleas__Approx_models (void) {
  //Arr[Model]
  Arr *mds = arr_new();
  arr_push(mds, approx__4());
  arr_push(mds, approx__3A());
  arr_push(mds, approx__3B());
  arr_push(mds, approx__2());
  return mds;
}

