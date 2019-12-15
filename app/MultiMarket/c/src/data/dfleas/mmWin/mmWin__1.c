// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/dfleas/mmWin/mmWin__1.h"
#include "dmc/Darr.h"
#include "data/dfleas/mmWin/MMWinBase.h"
#include "data/dfleas/dfleas__fmt.h"

enum {STEP};

// Step to operate is q + q * x, where x >= MAX_STEP && x <= MIN_STEP
#define MAX_STEP 0.10

// Step to operate is q + q * x, where x >= MAX_STEP && x <= MIN_STEP
#define MIN_STEP 0.005

static Darr *fparams(Flea *f) {
  Gen *g = flea_gen(f);
  double *p = gen_values(g);
  Darr *r = darr_new();

  darr_push(r, flea_param(MAX_STEP, MIN_STEP, *p++));

  return r;
}

// Arr[MMBackBase]
static Arr *fcos(Darr *params, int qnicks, QmatrixValues *closes) {
  return mMWinBase_cos(qnicks, closes, 0);
}

static Order *order(Darr *params, void *company, double q) {
  return mMWinBase_order(
    company,
    q,
    0,
    darr_get(params, STEP),
    0,
    darr_get(params, STEP)
  );
}

static double ref(Darr *params, void *co) {
  return mMWinBase_ref(co);
}

///
Model *mmWin__1 (void) {
  // Arr[ModelMxMn]
  Arr *param_cf = arr_new();
  arr_push(param_cf, modelMxMn_new("Paso", MAX_STEP, MIN_STEP));

  // Arr[Js]
  Arr *param_jss_js = arr_new();
  arr_push(param_jss_js, dfleas__fmt_perc());
  Js *param_jss = js_wa(param_jss_js);

  return model_new(
    str_new("MMWin1"),
    param_cf,
    param_jss,
    fparams,
    fcos,
    order,
    ref
  );

}

