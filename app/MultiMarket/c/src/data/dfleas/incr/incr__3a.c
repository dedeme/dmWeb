// Copyright 01-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/dfleas/incr/incr__3a.h"
#include "dmc/Darr.h"
#include "data/dfleas/incr/IncrBase.h"
#include "data/dfleas/dfleas__fmt.h"

enum {STRIP_TO_BUY, STRIP_TO_SELL};

#define DAYS 53

// Strip is d + d * x where x >= MAX_STRIP && x <= MIN_STRIP
#define MAX_STRIP_BUY 0.25

// Strip is d + d * x where x >= MAX_STRIP && x <= MIN_STRIP
#define MIN_STRIP_BUY 0.0001

// Strip is d + d * x where x >= MAX_STRIP && x <= MIN_STRIP
#define MAX_STRIP_SELL 0.25

// Strip is d + d * x where x >= MAX_STRIP && x <= MIN_STRIP
#define MIN_STRIP_SELL 0.0001

static Darr *fparams(Flea *f) {
  Gen *g = flea_gen(f);
  double *p = gen_values(g);
  Darr *r = darr_new();

  darr_push(r, flea_param(MAX_STRIP_BUY, MIN_STRIP_BUY, *p++));
  darr_push(r, flea_param(MAX_STRIP_SELL, MIN_STRIP_SELL, *p++));

  return r;
}

// Arr[IncrBase]
static Arr *fcos(Darr *params, int qnicks, QmatrixValues *closes) {
  return incrBase_cos(qnicks, closes, darr_get(params, STRIP_TO_SELL));
}

static Order *order(Darr *params, void *company, double q) {
  return incrBase_order(
    company,
    q,
    DAYS,
    darr_get(params, STRIP_TO_BUY),
    darr_get(params, STRIP_TO_SELL)
  );
}

static double ref(Darr *params, void *company) {
  return incrBase_ref(
    company,
    darr_get(params, STRIP_TO_BUY),
    darr_get(params, STRIP_TO_SELL)
  );
}

Model *incr__3a (void) {
  // Arr[ModelMxMn]
  Arr *param_cf = arr_new();
  arr_push(param_cf, modelMxMn_new(
    str_f("(D. %d)Banda C", DAYS), MAX_STRIP_BUY, MIN_STRIP_BUY
  ));
  arr_push(param_cf, modelMxMn_new(
    str_f("(D. %d)Banda V", DAYS), MAX_STRIP_SELL, MIN_STRIP_SELL
  ));

  // Arr[Js]
  Arr *param_jss_js = arr_new();
  arr_push(param_jss_js, dfleas__fmt_perc());
  arr_push(param_jss_js, dfleas__fmt_perc());
  Js *param_jss = js_wa(param_jss_js);


  return model_new(
    str_new("Incr3a"),
    param_cf,
    param_jss,
    fparams,
    fcos,
    order,
    ref
  );
}

