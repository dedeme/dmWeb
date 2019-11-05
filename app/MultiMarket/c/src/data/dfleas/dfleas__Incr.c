// Copyright 01-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/dfleas/dfleas__Incr.h"
#include "data/Model.h"
#include "data/dfleas/incr/incr__3.h"
#include "data/dfleas/incr/incr__2.h"
#include "data/dfleas/incr/incr__3a53.h"
#include "data/dfleas/incr/incr__3a58.h"
#include "data/dfleas/incr/incr__3aa.h"
#include "data/dfleas/incr/incr__3ab.h"
#include "data/dfleas/incr/incr__3a58a.h"
#include "data/dfleas/incr/incr__3a58b.h"

// Returns Arr[Model]
Arr *dfleas__Incr_models (void) {
  //Arr[Model]
  Arr *mds = arr_new();
  arr_push(mds, incr__3());
  arr_push(mds, incr__2());
  arr_push(mds, incr__3a53());
  arr_push(mds, incr__3a58());
  arr_push(mds, incr__3aa());
  arr_push(mds, incr__3ab());
  arr_push(mds, incr__3a58a());
  arr_push(mds, incr__3a58b());
  return mds;
}
