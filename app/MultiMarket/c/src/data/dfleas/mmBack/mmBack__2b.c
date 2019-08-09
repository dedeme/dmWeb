// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/dfleas/mmBack/mmBack__2b.h"
#include "dmc/Darr.h"
#include "data/dfleas/mmBack/MMBackBase.h"
#include "data/dfleas/dfleas__fmt.h"

enum {STEP};

#define STEP_TO_SELL_VALUE 0.05

// Step to operate is q + q * x, where x >= MAX_STEP && x <= MIN_STEP
#define MAX_STEP 0.07

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
  return mMBackBase_cos(qnicks, closes);
}

static Order *order(Darr *params, void *company, double q) {
  return mMBackBase_order(
    company,
    q,
    darr_get(params, STEP),
    STEP_TO_SELL_VALUE
  );
}

static double ref(Darr *params, void *co) {
  return ((MMBackBase *)co)->ref;
}

///
Model *mmBack__2b (void) {
  // Arr[ModelMxMn]
  Arr *param_cf = arr_new();
  arr_push(param_cf, modelMxMn_new(
    str_f("(V. %.2f%%) Paso C", STEP_TO_SELL_VALUE * 100),
    MAX_STEP, MIN_STEP
  ));

  // Arr[Js]
  Arr *param_jss_js = arr_new();
  arr_push(param_jss_js, dfleas__fmt_perc());
  Js *param_jss = js_wa(param_jss_js);

  return model_new(
    str_new("MMBack2b"),
    param_cf,
    param_jss,
    fparams,
    fcos,
    order,
    ref
  );

}

