// Copyright 20-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "scheduler/fleas/fleas__MA2.h"
#include "data/Flea.h"
#include "data/Order.h"
#include "DEFS.h"

enum {DAYS, STRIP_TO_BUY, STRIP_TO_SELL};

// Days is at d + d * x, where x >= MAX_DAYS && x <= MIN_DAYS
#define MAX_DAYS 120

// Days is at d + d * x, where x >= MAX_DAYS && x <= MIN_DAYS
#define MIN_DAYS 20

// Strip is d + d * x where x >= MAX_STRIP && x <= MIN_STRIP
#define MAX_STRIP 0.2

// Strip is d + d * x where x >= MAX_STRIP && x <= MIN_STRIP
#define MIN_STRIP 0.0001

#define CO struct minMax_co
CO {
  int to_sell;
  double avg;
  int days_ix;
  double ref;
};

static CO *co_new() {
  CO *this = malloc(sizeof(CO));
  this->to_sell = 1;
  this->days_ix = 0;
  this->avg = 0;
  this->ref = 0;
  return this;
}

static Darr *fparams(Flea *f) {
  Gen *g = flea_gen(f);
  double *p = gen_values(g);
  Darr *r = darr_new();

  darr_push(r, (double)(int)(flea_param(MAX_DAYS, MIN_DAYS, *p++)));
  darr_push(r, flea_param(MAX_STRIP, MIN_STRIP, *p++));
  darr_push(r, flea_param(MAX_STRIP, MIN_STRIP, *p++));

  return r;
}

// Arr[CO]
static Arr *fcos(Darr *params, int qnicks, QmatrixValues *closes) {
  Arr *r = arr_new();
  RANGE0(i, qnicks)
    arr_push(r, co_new());
  _RANGE
  return r;
}

static Order *order(Darr *params, void *company, double q) {
  CO *co = (CO *)company;
  int days = darr_get(params, DAYS);

  if (co->days_ix < days) {
    if (q > 0) {
      co->avg = (co->avg * co->days_ix + q) / (co->days_ix + 1);
      co->days_ix += 1;
      co->ref = co->avg;
    }
    return order_none();
  }

  if (q > 0) {
    double avg = (co->avg * (days - 1) + q) / days;
    co->avg = avg;
    if (co->to_sell) {
      double ref = co->ref;
      if (avg > ref) {
        co->ref = avg;
      } else {
        avg = ref;
      }
      if (q <= avg * (1 - darr_get(params, STRIP_TO_SELL))) {
        co->ref = co->avg;
        co->to_sell = 0;
        return order_sell();
      } else {
        return order_none();
      }
    } else {
      double ref0 = co->ref;
      if (avg < ref0) {
        co->ref = avg;
      } else {
        avg = ref0;
      }
      double ref = avg * (1 + darr_get(params, STRIP_TO_BUY));
      if (q >= ref) {
        double pond = (q - ref) / ref;
        co->ref = co->avg;
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

static double ref(Darr *params, void *company) {
  CO *co = (CO *)company;
  return co->to_sell
    ? co->ref  * (1 - darr_get(params, STRIP_TO_SELL))
    : co->ref * (1 + darr_get(params, STRIP_TO_BUY))
  ;
}

Model *fleas__MA2() {
  // Arr[char]
  Arr *param_names = arr_new();
  arr_push(param_names, "Días");
  arr_push(param_names, "Banda C");
  arr_push(param_names, "Banda V");

  // Arr[Js]
  Arr *param_jss_js = arr_new();
  Js *mk_pday_jss () {
    Arr *r = arr_new();
    arr_push(r, js_ws(""));
    arr_push(r, js_wi(1));
    arr_push(r, js_wi(0));
    arr_push(r, js_ws(""));
    Js *js = js_wa(r);
    return js;
  }
  Js *mk_p_jss () {
    Arr *r = arr_new();
    arr_push(r, js_ws(""));
    arr_push(r, js_wi(100));
    arr_push(r, js_wi(4));
    arr_push(r, js_ws("%"));
    Js *js = js_wa(r);
    return js;
  }
  arr_push(param_jss_js, mk_pday_jss());
  arr_push(param_jss_js, mk_p_jss());
  arr_push(param_jss_js, mk_p_jss());
  Js *param_jss = js_wa(param_jss_js);

  return model_new(
    str_new("MA2"),
    param_names,
    param_jss,
    fparams,
    fcos,
    order,
    ref
  );
}

#undef CO

