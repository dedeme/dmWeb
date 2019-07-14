// Copyright 20-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/dfleas/dfleas__Approx2.h"
#include "data/Flea.h"
#include "data/Order.h"
#include "DEFS.h"

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

#define CO struct approx_co
CO {
  int to_sell;
  double ref;
};

static CO *co_new(int to_sell, double ref) {
  CO *this = malloc(sizeof(CO));
  this->to_sell = to_sell;
  this->ref = ref;
  return this;
}

static Darr *fparams_new(Flea *f) {
  Gen *g = flea_gen(f);
  double *p = gen_values(g);
  Darr *r = darr_new();

  darr_push(r, flea_param(MAX_TO_START, MIN_TO_START, *p++));
  darr_push(r, flea_param(MAX_TO_STEP, MIN_TO_STEP, *p++));

  return r;
}

// Arr[CO]
static Arr *fcos(Darr *params, int qnicks, QmatrixValues *closes) {
  double start_to_sell = darr_get(params, START);
  Arr *r = arr_new();
  QmatrixValues *p;
  RANGE0(nk, qnicks)
    p = closes;
    double q = (*p++)[nk];
    while (q < 0) {
      q = (*p++)[nk];
    }
    arr_push(r, co_new(1, q * (1 - start_to_sell)));
  _RANGE
  return r;
}

static Order *order(Darr *params, void *company, double q) {
  CO *co = (CO *)company;
  if (q > 0) {
    if (co->to_sell) {
      co->ref = co->ref + (q - co->ref) * darr_get(params, STEP);
      if (q <= co->ref) {
        co->ref = q * (1 + darr_get(params, START));
        co->to_sell = 0;
        return order_sell();
      } else {
        return order_none();
      }
    } else {
      co->ref = co->ref - (co->ref - q) * darr_get(params, STEP);
      if (q >= co->ref) {
        double pond = (q - co->ref) / co->ref;
        co->ref = q * (1 - darr_get(params, START));
        co->to_sell = 1;
        return order_buy(pond);
      } else {
        return order_none();
      }
    }
  } else {
    return order_none();
  }
}

static double ref(Darr *params, void *co) {
  return ((CO *)co)->ref;
}

Model *dfleas__Approx2() {
  // Arr[ModelMxMn]
  Arr *param_cf = arr_new();
  arr_push(param_cf, modelMxMn_new("Inicio", MAX_TO_START, MIN_TO_START));
  arr_push(param_cf, modelMxMn_new("Paso", MAX_TO_STEP, MIN_TO_STEP));

  // Arr[Js]
  Arr *param_jss_js = arr_new();
  Js *mk_p_jss () {
    Arr *r = arr_new();
    arr_push(r, js_ws(""));
    arr_push(r, js_wi(100));
    arr_push(r, js_wi(4));
    arr_push(r, js_ws("%"));
    Js *js = js_wa(r);
    return js;
  }
  arr_push(param_jss_js, mk_p_jss());
  arr_push(param_jss_js, mk_p_jss());
  Js *param_jss = js_wa(param_jss_js);

  return model_new(
    str_new("Approx2"),
    param_cf,
    param_jss,
    fparams_new,
    fcos,
    order,
    ref
  );
}

#undef CO



