// Copyright 01-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/dfleas/incr/incr__2.h"
#include "dmc/Darr.h"
#include "data/dfleas/incr/IncrBase.h"
#include "data/dfleas/dfleas__fmt.h"

enum {DAYS, STRIP};

// Days is at d + d * x, where x >= MAX_DAYS && x <= MIN_DAYS
#define MAX_DAYS 120

// Days is at d + d * x, where x >= MAX_DAYS && x <= MIN_DAYS
#define MIN_DAYS 20

// Strip is d + d * x where x >= MAX_STRIP && x <= MIN_STRIP
#define MAX_STRIP 0.25

// Strip is d + d * x where x >= MAX_STRIP && x <= MIN_STRIP
#define MIN_STRIP 0.0001

static Darr *fparams(Flea *f) {
  Gen *g = flea_gen(f);
  double *p = gen_values(g);
  Darr *r = darr_new();

  darr_push(r, (double)(int)(flea_param(MAX_DAYS, MIN_DAYS, *p++)));
  darr_push(r, flea_param(MAX_STRIP, MIN_STRIP, *p++));

  return r;
}

// Arr[IncrBase]
static Arr *fcos(Darr *params, int qnicks, QmatrixValues *closes) {
  return incrBase_cos(qnicks, closes, darr_get(params, STRIP));
}

static Order *order(Darr *params, void *company, double q) {
  return incrBase_order(
    company,
    q,
    darr_get(params, DAYS),
    darr_get(params, STRIP),
    darr_get(params, STRIP)
  );
}

static double ref(Darr *params, void *company) {
  return incrBase_ref(
    company,
    darr_get(params, STRIP),
    darr_get(params, STRIP)
  );
}

Model *incr__2 (void) {
  // Arr[ModelMxMn]
  Arr *param_cf = arr_new();
  arr_push(param_cf, modelMxMn_new("Días", MAX_DAYS, MIN_DAYS));
  arr_push(param_cf, modelMxMn_new("Banda", MAX_STRIP, MIN_STRIP));

  // Arr[Js]
  Arr *param_jss_js = arr_new();
  arr_push(param_jss_js, dfleas__fmt_day());
  arr_push(param_jss_js, dfleas__fmt_perc());
  Js *param_jss = js_wa(param_jss_js);


  return model_new(
    str_new("Incr2"),
    param_cf,
    param_jss,
    fparams,
    fcos,
    order,
    ref
  );
}
