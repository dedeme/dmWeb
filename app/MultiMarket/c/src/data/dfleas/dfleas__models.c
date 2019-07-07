// Copyright 12-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/dfleas/dfleas__models.h"
#include "data/Model.h"
#include "data/dfleas/dfleas__Approx.h"
#include "data/dfleas/dfleas__Approx2.h"
#include "data/dfleas/dfleas__Approx3A.h"
#include "data/dfleas/dfleas__Approx3B.h"
#include "data/dfleas/dfleas__GA.h"
#include "data/dfleas/dfleas__Incr2.h"
#include "data/dfleas/dfleas__Incr3.h"
#include "data/dfleas/dfleas__Incr3a.h"
#include "data/dfleas/dfleas__MA.h"
#include "data/dfleas/dfleas__MA2.h"
#include "data/dfleas/dfleas__MM1.h"
#include "data/dfleas/dfleas__MM2A.h"
#include "data/dfleas/dfleas__MM2Aa.h"
#include "data/dfleas/dfleas__MM2Ab.h"
#include "data/dfleas/dfleas__MM2B.h"
#include "data/dfleas/dfleas__MM4.h"
#include "data/dfleas/dfleas__MMBack1.h"
#include "data/dfleas/dfleas__MMBack2.h"
#include "data/dfleas/dfleas__MMBack2a.h"
#include "data/dfleas/dfleas__MMBack2b.h"
#include "data/dfleas/dfleas__MMWin1.h"
#include "data/dfleas/dfleas__MMWin2.h"
#include "data/dfleas/dfleas__MMWin2a.h"
#include "data/dfleas/dfleas__MMWin2b.h"

// Arr[Model]
static Arr *models = NULL;

void dfleas__models_init (void) {
  models = arr_new();

  arr_push(models, dfleas__Approx());
  arr_push(models, dfleas__Approx2());
  arr_push(models, dfleas__Approx3A());
  arr_push(models, dfleas__Approx3B());
  arr_push(models, dfleas__GA());
  arr_push(models, dfleas__Incr2());
  arr_push(models, dfleas__Incr3());
//  arr_push(models, dfleas__Incr3a());
  arr_push(models, dfleas__MA());
  arr_push(models, dfleas__MA2());
  arr_push(models, dfleas__MM1());
  arr_push(models, dfleas__MM2A());
//  arr_push(models, dfleas__MM2Aa());
//  arr_push(models, dfleas__MM2Ab());
  arr_push(models, dfleas__MM2B());
  arr_push(models, dfleas__MM4());
  arr_push(models, dfleas__MMBack1());
  arr_push(models, dfleas__MMBack2());
//  arr_push(models, dfleas__MMBack2a());
//  arr_push(models, dfleas__MMBack2b());
  arr_push(models, dfleas__MMWin1());
  arr_push(models, dfleas__MMWin2());
//  arr_push(models, dfleas__MMWin2a());
//  arr_push(models, dfleas__MMWin2b());
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

ModelParams *dfleas__models_acc (void) {
  Darr *ps = darr_new();
  darr_push(ps, 0.018827);
  return modelParams_new(dfleas__MMBack1(), ps);
}
