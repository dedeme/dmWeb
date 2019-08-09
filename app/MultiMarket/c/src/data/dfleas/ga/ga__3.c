// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/dfleas/ga/ga__3.h"
#include "dmc/Darr.h"
#include "data/dfleas/ga/GABase.h"
#include "data/dfleas/dfleas__fmt.h"

enum {DAYS, STRIP_TO_BUY, STRIP_TO_SELL};

// Days is at d + d * x, where x >= MAX_DAYS && x <= MIN_DAYS
#define MAX_DAYS 120

// Days is at d + d * x, where x >= MAX_DAYS && x <= MIN_DAYS
#define MIN_DAYS 20

// Strip is d + d * x where x >= MAX_STRIP && x <= MIN_STRIP
#define MAX_STRIP 0.2

// Strip is d + d * x where x >= MAX_STRIP && x <= MIN_STRIP
#define MIN_STRIP 0.0001

static Darr *fparams(Flea *f) {
  Gen *g = flea_gen(f);
  double *p = gen_values(g);
  Darr *r = darr_new();

  darr_push(r, (double)(int)(flea_param(MAX_DAYS, MIN_DAYS, *p++)));
  darr_push(r, flea_param(MAX_STRIP, MIN_STRIP, *p++));
  darr_push(r, flea_param(MAX_STRIP, MIN_STRIP, *p++));

  return r;
}

// Arr[GABase]
static Arr *fcos(Darr *params, int qnicks, QmatrixValues *closes) {
  return gABase_cos(qnicks, closes, darr_get(params, DAYS));
}

static Order *order(Darr *params, void *company, double q) {
  return gABase_order(
    company,
    q,
    darr_get(params, DAYS),
    darr_get(params, STRIP_TO_BUY),
    darr_get(params, STRIP_TO_SELL)
  );
}

static double ref(Darr *params, void *company) {
  return gABase_ref(
    company,
    darr_get(params, STRIP_TO_BUY),
    darr_get(params, STRIP_TO_SELL)
  );
}

Model *ga__3() {
  // Arr[ModelMxMn]
  Arr *param_cf = arr_new();
  arr_push(param_cf, modelMxMn_new("Días", MAX_DAYS, MIN_DAYS));
  arr_push(param_cf, modelMxMn_new("Banda C", MAX_STRIP, MIN_STRIP));
  arr_push(param_cf, modelMxMn_new("Banda V", MAX_STRIP, MIN_STRIP));

  // Arr[Js]
  Arr *param_jss_js = arr_new();
  arr_push(param_jss_js, dfleas__fmt_day());
  arr_push(param_jss_js, dfleas__fmt_perc());
  arr_push(param_jss_js, dfleas__fmt_perc());
  Js *param_jss = js_wa(param_jss_js);

  return model_new(
    str_new("GA3"),
    param_cf,
    param_jss,
    fparams,
    fcos,
    order,
    ref
  );
}

