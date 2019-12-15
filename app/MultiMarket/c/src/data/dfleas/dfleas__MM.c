// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/dfleas/dfleas__MM.h"
#include "data/Model.h"
#include "data/dfleas/mm/mm__4.h"
#include "data/dfleas/mm/mm__2A.h"
#include "data/dfleas/mm/mm__2B.h"
#include "data/dfleas/mm/mm__1.h"

// Returns Arr[Model]
Arr *dfleas__MM_models (void) {
  //Arr[Model]
  Arr *mds = arr_new();
  arr_push(mds, mm__4());
  arr_push(mds, mm__2A());
  arr_push(mds, mm__2B());
  arr_push(mds, mm__1());
  return mds;
}

