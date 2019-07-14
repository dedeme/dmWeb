// Copyright 20-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/dfleas/dfleas__Approx.h"
#include "data/Flea.h"
#include "data/Order.h"
#include "DEFS.h"

enum {START_TO_BUY, STEP_TO_BUY, START_TO_SELL, STEP_TO_SELL};

// Start is at q + q * x, where x >= maxToBuy && x <= minToBuy
#define MAX_TO_BUY 0.30

// Start is at q + q * x, where x >= maxToBuy && x <= minToBuy
#define MIN_TO_BUY 0.01

// Step is ref - (q - ref) * x where x >= maxToBuyStep &&
// x <= minToBuyStep
#define MAX_TO_BUY_STEP 0.10

// Step is ref - (q - ref) * x where x >= maxToBuyStep &&
// x <= minToBuyStep
#define MIN_TO_BUY_STEP 0.005

// Start is at q - q * x, where x >= maxToSell && x <= minToSell
#define MAX_TO_SELL 0.30

// Start is at q - q * x, where x >= maxToSell && x <= minToSell
#define MIN_TO_SELL 0.01

// Step is ref + (q - ref) * x where x >= maxToSellStep &&
// x <= minToSellStep
#define MAX_TO_SELL_STEP 0.10

// Step is ref + (q - ref) * x where x >= maxToSellStep &&
// x <= minToSellStep
#define MIN_TO_SELL_STEP 0.005

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

static Darr *fparams(Flea *f) {
  Gen *g = flea_gen(f);
  double *p = gen_values(g);
  Darr *r = darr_new();

  darr_push(r, flea_param(MAX_TO_BUY, MIN_TO_BUY, *p++));
  darr_push(r, flea_param(MAX_TO_BUY_STEP, MIN_TO_BUY_STEP, *p++));
  darr_push(r, flea_param(MAX_TO_SELL, MIN_TO_SELL, *p++));
  darr_push(r, flea_param(MAX_TO_SELL_STEP, MIN_TO_SELL_STEP, *p++));

  return r;
}

// Arr[CO]
static Arr *fcos(Darr *params, int qnicks, QmatrixValues *closes) {
  double start_to_sell = darr_get(params, START_TO_SELL);
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
      co->ref = co->ref + (q - co->ref) * darr_get(params, STEP_TO_SELL);
      if (q <= co->ref) {
        co->ref = q * (1 + darr_get(params, START_TO_BUY));
        co->to_sell = 0;
        return order_sell();
      } else {
        return order_none();
      }
    } else {
      co->ref = co->ref - (co->ref - q) * darr_get(params, STEP_TO_BUY);
      if (q >= co->ref) {
        double pond = (q - co->ref) / co->ref;
        co->ref = q * (1 - darr_get(params, START_TO_SELL));
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

Model *dfleas__Approx() {
  // Arr[ModelMxMn]
  Arr *param_cf = arr_new();
  arr_push(param_cf, modelMxMn_new("Inicio C", MAX_TO_BUY, MIN_TO_BUY));
  arr_push(param_cf, modelMxMn_new("Paso C", MAX_TO_BUY_STEP, MIN_TO_BUY_STEP));
  arr_push(param_cf, modelMxMn_new("Inicio V", MAX_TO_SELL, MIN_TO_SELL));
  arr_push(param_cf, modelMxMn_new(
    "Paso V", MAX_TO_SELL_STEP, MIN_TO_SELL_STEP
  ));

  // Arr[Js]
  Arr *param_jss_js = arr_new();
  Js *mk_p_jss_new () {
    Arr *r = arr_new();
    arr_push(r, js_ws(""));
    arr_push(r, js_wi(100));
    arr_push(r, js_wi(4));
    arr_push(r, js_ws("%"));
    Js *js = js_wa(r);
    return js;
  }
  arr_push(param_jss_js, mk_p_jss_new());
  arr_push(param_jss_js, mk_p_jss_new());
  arr_push(param_jss_js, mk_p_jss_new());
  arr_push(param_jss_js, mk_p_jss_new());
  Js *param_jss = js_wa(param_jss_js);

  return model_new(
    str_new("Approx"),
    param_cf,
    param_jss,
    fparams,
    fcos,
    order,
    ref
  );
}

#undef CO

