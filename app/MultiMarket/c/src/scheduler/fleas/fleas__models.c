// Copyright 12-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "scheduler/fleas/fleas__models.h"
#include "data/Model.h"
#include "scheduler/fleas/fleas__Approx.h"
#include "scheduler/fleas/fleas__Approx2.h"
#include "scheduler/fleas/fleas__Approx3a.h"
#include "scheduler/fleas/fleas__Approx3b.h"
#include "scheduler/fleas/fleas__GA.h"
#include "scheduler/fleas/fleas__Incr2.h"
#include "scheduler/fleas/fleas__Incr3.h"
#include "scheduler/fleas/fleas__MA.h"
#include "scheduler/fleas/fleas__MA2.h"
#include "scheduler/fleas/fleas__MM1.h"
#include "scheduler/fleas/fleas__MM2A.h"
#include "scheduler/fleas/fleas__MM2Aa.h"
#include "scheduler/fleas/fleas__MM2Ab.h"
#include "scheduler/fleas/fleas__MM2B.h"
#include "scheduler/fleas/fleas__MM4.h"
#include "scheduler/fleas/fleas__MMBack1.h"
#include "scheduler/fleas/fleas__MMBack2.h"
#include "scheduler/fleas/fleas__MMBack2a.h"
#include "scheduler/fleas/fleas__MMBack2b.h"
#include "scheduler/fleas/fleas__MMWin1.h"
#include "scheduler/fleas/fleas__MMWin2.h"
#include "scheduler/fleas/fleas__MMWin2a.h"
#include "scheduler/fleas/fleas__MMWin2b.h"

// Arr[Model]
static Arr *models = NULL;

void fleas__models_init (void) {
  models = arr_new();

  arr_push(models, fleas__Approx());
  arr_push(models, fleas__Approx2());
  arr_push(models, fleas__Approx3a());
  arr_push(models, fleas__Approx3b());
  arr_push(models, fleas__GA());
  arr_push(models, fleas__Incr2());
  arr_push(models, fleas__Incr3());
  arr_push(models, fleas__MA());
  arr_push(models, fleas__MA2());
  arr_push(models, fleas__MM1());
  arr_push(models, fleas__MM2A());
  arr_push(models, fleas__MM2Aa());
  arr_push(models, fleas__MM2Ab());
  arr_push(models, fleas__MM2B());
  arr_push(models, fleas__MM4());
  arr_push(models, fleas__MMBack1());
  arr_push(models, fleas__MMBack2());
  arr_push(models, fleas__MMBack2a());
  arr_push(models, fleas__MMBack2b());
  arr_push(models, fleas__MMWin1());
  arr_push(models, fleas__MMWin2());
  arr_push(models, fleas__MMWin2a());
  arr_push(models, fleas__MMWin2b());
}

// Arr[Model]
Arr *fleas__models (void) {
  if (!models) EXC_ILLEGAL_STATE("'models' was not intiliazed")

  return models;
}

// Arr[char]
Arr *fleas__models_names (void) {
  // Arr[char]
  Arr *r = arr_new();
  EACH(fleas__models(), Model, m)
    arr_push(r, model_name(m));
  _EACH
  return r;
}

// Opt[Model]
Opt *fleas__models_get (char *name) {
  int fn (Model *m) { return str_eq(model_name(m), name); }
  return it_find(arr_to_it(fleas__models()), (FPRED)fn);
}

ModelParams *fleas__models_acc (void) {
  Darr *ps = darr_new();
  darr_push(ps, 0.018827);
  return modelParams_new(fleas__MMBack1(), ps);
}
