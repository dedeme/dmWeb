// Copyright 12-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/dfleas/dfleas__models.h"
#include "data/Model.h"
#include "data/dfleas/dfleas__Approx.h"
#include "data/dfleas/dfleas__GA.h"
#include "data/dfleas/dfleas__Incr.h"
#include "data/dfleas/dfleas__IncrMM.h"
#include "data/dfleas/dfleas__MA.h"
#include "data/dfleas/dfleas__MM.h"
#include "data/dfleas/dfleas__MMBack.h"
#include "data/dfleas/dfleas__MMWin.h"

// Arr[Model]
static Arr *models = NULL;

void dfleas__models_init (void) {
  models = arr_new();

  // 'a' is Arr[Model]
  void add (Arr *a) {
    EACH(a, Model, m)
      arr_push(models, m);
    _EACH
  }

  add(dfleas__Incr_models());
  add(dfleas__IncrMM_models());
  add(dfleas__Approx_models());
  add(dfleas__GA_models());
  add(dfleas__MA_models());
  add(dfleas__MM_models());
  add(dfleas__MMBack_models());
  add(dfleas__MMWin_models());

  int fn (Model *m1, Model *m2) {
    return str_greater(model_name(m1), model_name(m2));
  }
  arr_sort(models, (FCMP)fn);
}

// Arr[Model]
Arr *dfleas__models (void) {
  if (!models) EXC_ILLEGAL_STATE("'models' was not intiliazed")

  return models;
}

// Arr[char]
Arr *dfleas__models_names (void) {
  // Arr[char]
  Arr *r = arr_new();
  EACH(dfleas__models(), Model, m)
    arr_push(r, model_name(m));
  _EACH
  return r;
}

// Opt[Model]
Opt *dfleas__models_get (char *name) {
  int fn (Model *m) { return str_eq(model_name(m), name); }
  return it_find(arr_to_it(dfleas__models()), (FPRED)fn);
}

ModelParams *dfleas__models_default (void) {
  Darr *ps = darr_new();
  darr_push(ps, 0.018827);
  return modelParams_new(dfleas__MMBack_default(), ps);
}
