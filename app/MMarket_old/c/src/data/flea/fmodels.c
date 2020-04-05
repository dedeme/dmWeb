// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/flea/fmodels.h"
#include "data/flea/Fmodel.h"
#include "data/flea/models/Approx.h"

// Arr[Fmodel]
Arr *fmodels_list (void) {
  // Arr[Fmodel]
  Arr *r = arr_new();
  arr_push(r, approx_model());
  return r;
}

Opt *fmodels_get (char *model_id) {
  int fn (Fmodel *m) { return str_eq(fmodel_id(m), model_id); }
  return it_find(it_from(fmodels_list()), (FPRED)fn);
}
