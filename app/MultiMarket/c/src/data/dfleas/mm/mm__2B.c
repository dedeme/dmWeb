// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/dfleas/mm/mm__2B.h"
#include "dmc/Darr.h"
#include "data/dfleas/mm/MMBase.h"
#include "data/dfleas/dfleas__fmt.h"

enum {STRIP, STEP};

// Strip to buy is q * ( 1 + x), where x >= MIN_STRIP && x <= MAX_STRIP
#define MAX_STRIP 0.10

// Strip to buy is q * ( 1 + x), where x >= MIN_STRIP && x <= MAX_STRIP
#define MIN_STRIP 0.001

// Step to buy is q + (q - ref) * x, where x >= MIN_STEP && x <= MAX_STEP
#define MAX_STEP 0.10

// Step to buy is q + (q - ref) * x, where x >= MIN_STEP && x <= MAX_STEP
#define MIN_STEP 0.001

static Darr *fparams(Flea *f) {
  double *p = gen_values(flea_gen(f));
  Darr *r = darr_new();

  darr_push(r, flea_param(MAX_STRIP, MIN_STRIP, *p++));
  darr_push(r, flea_param(MAX_STEP, MIN_STEP, *p++));

  return r;
}

// Arr[MMBase]
static Arr *fcos(Darr *params, int qnicks, QmatrixValues *closes) {
  return mMBase_cos(qnicks, closes, darr_get(params, STRIP));
}

static Order *order(Darr *params, void *company, double q) {
  return mMBase_order(
    company,
    q,
    darr_get(params, STRIP),
    darr_get(params, STEP),
    darr_get(params, STRIP),
    darr_get(params, STEP)
  );
}

static double ref(Darr *params, void *co) {
  return ((MMBase *)co)->ref;
}

///
Model *mm__2B (void) {
  // Arr[ModelMxMn]
  Arr *param_cf = arr_new();
  arr_push(param_cf, modelMxMn_new("Banda", MAX_STRIP, MIN_STRIP));
  arr_push(param_cf, modelMxMn_new("Paso", MAX_STEP, MIN_STEP));

  // Arr[Js]
  Arr *param_jss_js = arr_new();
  arr_push(param_jss_js, dfleas__fmt_perc());
  arr_push(param_jss_js, dfleas__fmt_perc());
  Js *param_jss = js_wa(param_jss_js);

  return model_new(
    str_new("MM2B"),
    param_cf,
    param_jss,
    fparams,
    fcos,
    order,
    ref
  );

}

