// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/dfleas/mmWin/mmWin__2.h"
#include "dmc/Darr.h"
#include "data/dfleas/mmWin/MMWinBase.h"
#include "data/dfleas/dfleas__fmt.h"

enum {STEP_TO_BUY, STEP_TO_SELL};

// Step to buy is q + q * x, where x >= MAX_TO_BUY && x <= MAX_TO_BUY
#define MAX_TO_BUY 0.10

// Step to buy is q + q * x, where x >= MAX_TO_BUY && x <= MAX_TO_BUY
#define MIN_TO_BUY 0.001

// Step to sell is q + q * x, where x >= MAX_TO_SELL && x <= MIN_TO_SELL
#define MAX_TO_SELL 0.10

// Step to sell is q + q * x, where x >= MAX_TO_SELL && x <= MIN_TO_SELL
#define MIN_TO_SELL 0.001

static Darr *fparams(Flea *f) {
  Gen *g = flea_gen(f);
  double *p = gen_values(g);
  Darr *r = darr_new();

  darr_push(r, flea_param(MAX_TO_BUY, MIN_TO_BUY, *p++));
  darr_push(r, flea_param(MAX_TO_SELL, MIN_TO_SELL, *p++));

  return r;
}

// Arr[MMBackBase]
static Arr *fcos(Darr *params, int qnicks, QmatrixValues *closes) {
  return mMWinBase_cos(qnicks, closes);
}

static Order *order(Darr *params, void *company, double q) {
  return mMWinBase_order(
    company,
    q,
    darr_get(params, STEP_TO_BUY),
    darr_get(params, STEP_TO_SELL)
  );
}

static double ref(Darr *params, void *co) {
  return mMWinBase_ref(co);
}

///
Model *mmWin__2 (void) {
  // Arr[ModelMxMn]
  Arr *param_cf = arr_new();
  arr_push(param_cf, modelMxMn_new("Paso C", MAX_TO_BUY, MIN_TO_BUY));
  arr_push(param_cf, modelMxMn_new("Paso V", MAX_TO_SELL, MIN_TO_SELL));

  // Arr[Js]
  Arr *param_jss_js = arr_new();
  arr_push(param_jss_js, dfleas__fmt_perc());
  arr_push(param_jss_js, dfleas__fmt_perc());
  Js *param_jss = js_wa(param_jss_js);

  return model_new(
    str_new("MMWin2"),
    param_cf,
    param_jss,
    fparams,
    fcos,
    order,
    ref
  );

}
