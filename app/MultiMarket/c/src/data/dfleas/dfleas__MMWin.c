// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/dfleas/dfleas__MMWin.h"
#include "data/Model.h"
#include "data/dfleas/mmWin/mmWin__2.h"
#include "data/dfleas/mmWin/mmWin__2a.h"
#include "data/dfleas/mmWin/mmWin__2b.h"
#include "data/dfleas/mmWin/mmWin__1.h"

/// Returns Arr[Model]
Arr *dfleas__MMWin_models (void) {
  //Arr[Model]
  Arr *mds = arr_new();
  arr_push(mds, mmWin__2());
  arr_push(mds, mmWin__2a());
  arr_push(mds, mmWin__2b());
  arr_push(mds, mmWin__1());
  return mds;
}
