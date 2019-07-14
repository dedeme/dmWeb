// Copyright 20-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/dfleas/dfleas__MMWin2.h"
#include "data/Flea.h"
#include "data/Order.h"
#include "DEFS.h"

enum {STEP_TO_BUY, STEP_TO_SELL};

// Step to buy is q + q * x, where x >= MAX_TO_BUY && x <= MAX_TO_BUY
#define MAX_TO_BUY 0.10

// Step to buy is q + q * x, where x >= MAX_TO_BUY && x <= MAX_TO_BUY
#define MIN_TO_BUY 0.001

// Step to sell is q + q * x, where x >= MAX_TO_SELL && x <= MIN_TO_SELL
#define MAX_TO_SELL 0.10

// Step to sell is q + q * x, where x >= MAX_TO_SELL && x <= MIN_TO_SELL
#define MIN_TO_SELL 0.001


#define CO struct approx_co
CO {
  int to_sell;
  double ref;
  double mm;
  double ref0; // mm of day of last operation
  double qop; // close of day of last operation
  double qlast; // close of last day
};

static CO *co_new(int to_sell, double ref, double max) {
  CO *this = malloc(sizeof(CO));
  this->to_sell = to_sell;
  this->ref = ref;
  this->mm = max;
  this->ref0 = max;
  this->qop = max;
  this->qlast = max;
  return this;
}

static Darr *fparams(Flea *f) {
  Gen *g = flea_gen(f);
  double *p = gen_values(g);
  Darr *r = darr_new();

  darr_push(r, flea_param(MAX_TO_BUY, MIN_TO_BUY, *p++));
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
    arr_push(r, co_new(1, q * (0.95), q));
  _RANGE
  return r;
}

static Order *order(Darr *params, void *company, double q) {
  CO *co = (CO *)company;
  co->qlast = q;
  if (q > 0) {
    if (co->to_sell) {
      if (q > co->ref) {
        co->ref = co->ref + (q - co->ref) * darr_get(params, STEP_TO_SELL);
      }
      if (q <= co->ref && (q > co->qop || q < co->ref0)) {
        co->ref = co->mm;
        co->ref0 = co->mm;
        co->mm = q;
        co->qop = q;
        co->to_sell = 0;
        return order_sell();
      } else {
        co->mm = q > co->mm ? q : co->mm;
        return order_none();
      }
    } else {
      if (q < co->ref) {
        co->ref = co->ref - (co->ref - q) * darr_get(params, STEP_TO_BUY);
      }
      if (q >= co->ref && (q < co->qop || q > co->ref0)) {
        double pond = (q - co->ref) / co->ref;
        co->ref = co->mm;
        co->ref0 = co->mm;
        co->mm = q;
        co->qop = q;
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
  CO *c = (CO *) co;
  return
      (c->to_sell && c->ref > c->qop && c->ref < c->qlast) ||
      (!c->to_sell && c->ref < c->qop && c->ref > c->qlast)
    ? c->ref
    : c->ref0
  ;
}

Model *dfleas__MMWin2() {
  // Arr[ModelMxMn]
  Arr *param_cf = arr_new();
  arr_push(param_cf, modelMxMn_new("Paso C", MAX_TO_BUY, MIN_TO_BUY));
  arr_push(param_cf, modelMxMn_new("Paso V", MAX_TO_SELL, MIN_TO_SELL));

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
  Js *param_jss = js_wa(param_jss_js);

  return model_new(
    str_new("MMWin2"),
    param_cf,
    param_jss,
    fparams,
    fcos,
    order,
    ref
  );
}

#undef CO


