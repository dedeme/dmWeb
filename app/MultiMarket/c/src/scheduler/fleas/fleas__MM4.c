// Copyright 20-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "scheduler/fleas/fleas__MM4.h"
#include "data/Flea.h"
#include "data/Order.h"
#include "DEFS.h"

enum {STRIP_TO_BUY, STEP_TO_BUY, STRIP_TO_SELL, STEP_TO_SELL};

// Strip to buy is q * ( 1 + x), where x >= MIN_STRIP_TO_BUY &&
// x <= MAX_STRIP_TO_BUY
#define MAX_STRIP_TO_BUY 0.10

// Strip to buy is q * ( 1 + x), where x >= MIN_STRIP_TO_BUY &&
// x <= MAX_STRIP_TO_BUY
#define MIN_STRIP_TO_BUY 0.001

// Step to buy is q + (q - ref) * x, where x >= MIN_TO_BUY && x <= MAX_TO_BUY
#define MAX_TO_BUY 0.10

// Step to buy is q + (q - ref) * x, where x >= MIN_TO_BUY && x <= MAX_TO_BUY
#define MIN_TO_BUY 0.001

// Strip to sell is q * ( 1 - x), where x >= MIN_STRIP_TO_SELL &&
// x <= MAX_STRIP_TO_SELL
#define MAX_STRIP_TO_SELL 0.10

// Strip to sell is q * ( 1 - x), where x >= MIN_STRIP_TO_SELL &&
// x <= MAX_STRIP_TO_SELL
#define MIN_STRIP_TO_SELL 0.001

// Step to sell is q + (ref - q) * x, where x >= MIN_TO_SELL && x <= MIN_TO_SELL
#define MAX_TO_SELL 0.10

// Step to sell is q + (ref - q) * x, where x >= MIN_TO_SELL && x <= MIN_TO_SELL
#define MIN_TO_SELL 0.001


#define CO struct approx_co
CO {
  int to_sell;
  double ref;
  double mm;
};

static CO *co_new(int to_sell, double ref, double q) {
  CO *this = malloc(sizeof(CO));
  this->to_sell = to_sell;
  this->ref = ref;
  this->mm = q;
  return this;
}

static Darr *fparams(Flea *f) {
  Gen *g = flea_gen(f);
  double *p = gen_values(g);
  Darr *r = darr_new();

  darr_push(r, flea_param(MAX_STRIP_TO_BUY, MIN_STRIP_TO_BUY, *p++));
  darr_push(r, flea_param(MAX_TO_BUY, MIN_TO_BUY, *p++));
  darr_push(r, flea_param(MAX_STRIP_TO_SELL, MIN_STRIP_TO_SELL, *p++));
  darr_push(r, flea_param(MAX_TO_SELL, MIN_TO_SELL, *p++));

  return r;
}

// Arr[CO]
static Arr *fcos(Darr *params, int qnicks, QmatrixValues *closes) {
  Arr *r = arr_new();
  QmatrixValues *p;
  RANGE0(nk, qnicks)
    p = closes;
    double q = (*p++)[nk];
    while (q < 0) {
      q = (*p++)[nk];
    }
    arr_push(
      r,
      co_new(1, q * (0.95) * (1 - darr_get(params, STRIP_TO_SELL)) , q)
    );
  _RANGE
  return r;
}

static Order *order(Darr *params, void *company, double q) {
  CO *co = (CO *)company;
  if (q > 0) {
    if (co->to_sell) {
      co->ref = co->ref + (q - co->ref) * darr_get(params, STEP_TO_SELL);
      if (q <= co->ref) {
        co->ref = co->mm * (1 + darr_get(params, STRIP_TO_BUY));
        co->mm = q;
        co->to_sell = 0;
        return order_sell();
      } else {
        co->mm = q > co->mm ? q : co->mm;
        return order_none();
      }
    } else {
      co->ref = co->ref - (co->ref - q) * darr_get(params, STEP_TO_BUY);
      if (q >= co->ref) {
        double pond = (q - co->ref) / co->ref;
        co->ref = co->mm * (1 -darr_get(params, STRIP_TO_BUY));
        co->mm = q;
        co->to_sell = 1;
        return order_buy(pond);
      } else {
        co->mm = q < co->mm ? q : co->mm;
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

Model *fleas__MM4() {
  // Arr[char]
  Arr *param_names = arr_new();
  arr_push(param_names, "Banda C");
  arr_push(param_names, "Paso C");
  arr_push(param_names, "Banda V");
  arr_push(param_names, "Paso V");

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
  arr_push(param_jss_js, mk_p_jss());
  arr_push(param_jss_js, mk_p_jss());
  Js *param_jss = js_wa(param_jss_js);

  return model_new(
    str_new("MM4"),
    param_names,
    param_jss,
    fparams,
    fcos,
    order,
    ref
  );
}

#undef CO

