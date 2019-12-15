// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/dfleas/dfleas__MMBack.h"
#include "data/Model.h"
#include "data/dfleas/mmBack/mmBack__4.h"
#include "data/dfleas/mmBack/mmBack__2.h"
#include "data/dfleas/mmBack/mmBack__1.h"

/// Returns Arr[Model]
Arr *dfleas__MMBack_models (void) {
  //Arr[Model]
  Arr *mds = arr_new();
  arr_push(mds, mmBack__4());
  arr_push(mds, mmBack__2());
  arr_push(mds, mmBack__1());
  return mds;
}

/// Returns default model
Model *dfleas__MMBack_default (void) {
  return mmBack__1();
}
