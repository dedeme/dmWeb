// Copyright 21-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/dfleas/dfleas__Incr2.h"
#include "data/Flea.h"
#include "data/Order.h"
#include "DEFS.h"

enum {DAYS, STRIP};

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
  QmatrixValues *closes;
  double ref;
  int con; // Company number
  int closes_ix; // Index (day) of Qmatrix
  int days_ix;
};

static CO *co_new(int to_sell, QmatrixValues *closes, double ref, int con) {
  CO *this = malloc(sizeof(CO));
  this->to_sell = to_sell;
  this->closes = closes;
  this->ref = ref;
  this->con = con;
  this->closes_ix = 0;
  this->days_ix = 0;
  return this;
}

static Darr *fparams(Flea *f) {
  Gen *g = flea_gen(f);
  double *p = gen_values(g);
  Darr *r = darr_new();

  darr_push(r, (double)(int)(flea_param(MAX_DAYS, MIN_DAYS, *p++)));
  darr_push(r, flea_param(MAX_STRIP, MIN_STRIP, *p++));

  return r;
}

// Arr[CO]
static Arr *fcos(Darr *params, int qnicks, QmatrixValues *closes) {
  double strip_to_sell = darr_get(params, STRIP);
  Arr *r = arr_new();
  RANGE0(i, qnicks)
    arr_push(r, co_new(1, closes, closes[0][i] * (1 - strip_to_sell), i));
  _RANGE
  return r;
}

static Order *order(Darr *params, void *company, double q) {
  CO *co = (CO *)company;
  int days = darr_get(params, DAYS);

  if (co->days_ix < days) {
    co->days_ix += 1;
    return order_none();
  }

  ++co->closes_ix;
  double new_ref = co->closes[co->closes_ix][co->con];
  if (co->to_sell) {
    if (q > 0) {
      if (new_ref > 0 || co->ref > 0) {
        if (co->ref < 0 || new_ref > co->ref) {
          co->ref = new_ref;
        }
        if (q <= (co->ref * (1 - darr_get(params, STRIP)))) {
          co->ref = new_ref;
          co->to_sell = 0;
          return order_sell();
        } else {
          return order_none();
        }
      } else {
        return order_none();
      }
    } else {
      if ((co->ref > 0 && new_ref > co->ref) || co->ref <= 0) {
        co->ref = new_ref;
      }
      return order_none();
    }
  } else {
    if (q > 0) {
      if (new_ref > 0 || co->ref > 0) {
        if (co->ref < 0 || new_ref < co->ref) {
          co->ref = new_ref;
        }
        double ref = co->ref * (1 + darr_get(params, STRIP));
        if (q >= ref) {
          double pond = (q - ref) / ref;
          co->ref = new_ref;
          co->to_sell = 1;
          return order_buy(pond);
        } else {
          return order_none();
        }
      } else {
        return order_none();
      }
    } else {
      if ((co->ref > 0 && new_ref < co->ref) || co->ref <= 0) {
        co->ref = new_ref;
      }
      return order_none();
    }
  }
}

static double ref(Darr *params, void *company) {
  CO *co = (CO *)company;
  return co->to_sell
    ? co->ref * (1 - darr_get(params, STRIP))
    : co->ref * (1 + darr_get(params, STRIP))
  ;
}

Model *dfleas__Incr2() {
  // Arr[ModelMxMn]
  Arr *param_cf = arr_new();
  arr_push(param_cf, modelMxMn_new("Días", MAX_DAYS, MIN_DAYS));
  arr_push(param_cf, modelMxMn_new("Banda", MAX_STRIP, MIN_STRIP));

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

#undef CO


