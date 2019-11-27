// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/dfleas/approx/approx__3B.h"
#include "dmc/Darr.h"
#include "data/dfleas/approx/ApproxBase.h"
#include "data/dfleas/dfleas__fmt.h"

enum {START_TO_BUY, START_TO_SELL, STEP};

// Start is at q + q * x, where x >= maxToBuy && x <= minToBuy
#define MAX_TO_BUY 0.30

// Start is at q + q * x, where x >= maxToBuy && x <= minToBuy
#define MIN_TO_BUY 0.01

// Step is ref - (q - ref) * x where x >= maxToStep &&
// x <= minToStep
#define MAX_TO_STEP 0.10

// Step is ref - (q - ref) * x where x >= maxToStep &&
// x <= minToStep
#define MIN_TO_STEP 0.005

// Start is at q - q * x, where x >= maxToSell && x <= minToSell
#define MAX_TO_SELL 0.30

// Start is at q - q * x, where x >= maxToSell && x <= minToSell
#define MIN_TO_SELL 0.01

static Darr *fparams(Flea *f) {
  double *p = gen_values(flea_gen(f));
  Darr *r = darr_new();

  darr_push(r, flea_param(MAX_TO_BUY, MIN_TO_BUY, *p++));
  darr_push(r, flea_param(MAX_TO_SELL, MIN_TO_SELL, *p++));
  darr_push(r, flea_param(MAX_TO_STEP, MIN_TO_STEP, *p++));

  return r;
}

// Arr[AprroxBase]
static Arr *fcos(Darr *params, int qnicks, QmatrixValues *closes) {
  return approxBase_cos(qnicks, closes, darr_get(params, START_TO_SELL));
}

static Order *order(Darr *params, void *company, double q) {
  return approxBase_order(
    company,
    q,
    darr_get(params, START_TO_BUY),
    darr_get(params, STEP),
    darr_get(params, START_TO_SELL),
    darr_get(params, STEP)
  );
}

static double ref(Darr *params, void *co) {
  return ((ApproxBase *)co)->ref;
}

///
Model *approx__3B (void) {
  // Arr[ModelMxMn]
  Arr *param_cf = arr_new();
  arr_push(param_cf, modelMxMn_new("Inicio C", MAX_TO_BUY, MIN_TO_BUY));
  arr_push(param_cf, modelMxMn_new("Inicio V", MAX_TO_SELL, MIN_TO_SELL));
  arr_push(param_cf, modelMxMn_new("Paso", MAX_TO_STEP, MIN_TO_STEP));

  // Arr[Js]
  Arr *param_jss_js = arr_new();
  arr_push(param_jss_js, dfleas__fmt_perc());
  arr_push(param_jss_js, dfleas__fmt_perc());
  arr_push(param_jss_js, dfleas__fmt_perc());
  Js *param_jss = js_wa(param_jss_js);

  return model_new(
    str_new("Approx3B"),
    param_cf,
    param_jss,
    fparams,
    fcos,
    order,
    ref
  );

}