// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/dfleas/approx/approx__2.h"
#include "dmc/Darr.h"
#include "data/dfleas/approx/ApproxBase.h"
#include "data/dfleas/dfleas__fmt.h"

enum {START, STEP};

// Start is at q + q * x, where x >= MAX_TO_START && x <= MIN_TO_START
#define MAX_TO_START 0.30

// Start is at q + q * x, where x >= MAX_TO_START && x <= MIN_TO_START
#define MIN_TO_START 0.01

// Step is ref - (q - ref) * x where x >= MAX_TO_STEP &&
// x <= MAX_TO_STEP
#define MAX_TO_STEP 0.10

// Step is ref - (q - ref) * x where x >= MAX_TO_STEP &&
// x <= MAX_TO_STEP
#define MIN_TO_STEP 0.005

static Darr *fparams(Flea *f) {
  double *p = gen_values(flea_gen(f));
  Darr *r = darr_new();

  darr_push(r, flea_param(MAX_TO_START, MIN_TO_START, *p++));
  darr_push(r, flea_param(MAX_TO_STEP, MIN_TO_STEP, *p++));

  return r;
}

// Arr[AprroxBase]
static Arr *fcos(Darr *params, int qnicks, QmatrixValues *closes) {
  return approxBase_cos(qnicks, closes, darr_get(params, START));
}

static Order *order(Darr *params, void *company, double q) {
  return approxBase_order(
    company,
    q,
    darr_get(params, START),
    darr_get(params, STEP),
    darr_get(params, START),
    darr_get(params, STEP)
  );
}

static double ref(Darr *params, void *co) {
  return ((ApproxBase *)co)->ref;
}

///
Model *approx__2 (void) {
  // Arr[ModelMxMn]
  Arr *param_cf = arr_new();
  arr_push(param_cf, modelMxMn_new("Inicio", MAX_TO_START, MIN_TO_START));
  arr_push(param_cf, modelMxMn_new("Paso", MAX_TO_STEP, MIN_TO_STEP));

  // Arr[Js]
  Arr *param_jss_js = arr_new();
  arr_push(param_jss_js, dfleas__fmt_perc());
  arr_push(param_jss_js, dfleas__fmt_perc());
  Js *param_jss = js_wa(param_jss_js);

  return model_new(
    str_new("Approx2"),
    param_cf,
    param_jss,
    fparams,
    fcos,
    order,
    ref
  );

}

