// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/flea/Fmodel.h"
#include "dmc/rnd.h"
#include "data/broker.h"
#include "data/Nick.h"
#include "data/flea/Dorder.h"
#include "DEFS.h"

/* .
# Register to tests orders.
FmodelOrder: to
  date: char *
  nick: char *
  is_sell: bool
  stocks: int
  price: double
*/

/*--*/

struct Fmodel_FmodelOrder {
  char *date;
  char *nick;
  int is_sell;
  int stocks;
  double price;
};

FmodelOrder *fmodelOrder_new (
  char *date,
  char *nick,
  int is_sell,
  int stocks,
  double price
) {
  FmodelOrder *this = MALLOC(FmodelOrder);
  this->date = date;
  this->nick = nick;
  this->is_sell = is_sell;
  this->stocks = stocks;
  this->price = price;
  return this;
}

char *fmodelOrder_date (FmodelOrder *this) {
  return this->date;
}

char *fmodelOrder_nick (FmodelOrder *this) {
  return this->nick;
}

int fmodelOrder_is_sell (FmodelOrder *this) {
  return this->is_sell;
}

int fmodelOrder_stocks (FmodelOrder *this) {
  return this->stocks;
}

double fmodelOrder_price (FmodelOrder *this) {
  return this->price;
}

Js *fmodelOrder_to_js (FmodelOrder *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(this->date));
  arr_push(js, js_ws(this->nick));
  arr_push(js, js_wb(this->is_sell));
  arr_push(js, js_wi((int)this->stocks));
  arr_push(js, js_wd(this->price));
  return js_wa(js);
}

/*--*/

struct Fmodel_Fmodel {
  char *id;
  char *name;
  Arr *par_names; // Arr[char]
  Darr *par_mins;
  Darr *par_maxs;
  Arr *par_js_fmt; // Arr[char] - Parameters format to show in javascript.
                   // For example: "new Dec(#N#, 2).toIso() + '%'"
                   // Where '#N#' will be replaced by the parameter value.
  Fcalc fcalc;
};

Fmodel *fmodel_new (
  char *id,
  char *name,
  Arr *par_names, // Arr[char*]
  Darr *par_mins,
  Darr *par_maxs,
  Arr *par_js_fmt,
  Fcalc fcalc
) {
  Fmodel *this = MALLOC(Fmodel);
  this->id = id;
  this->name = name;
  this->par_names = par_names;
  this->par_mins = par_mins;
  this->par_maxs = par_maxs;
  this->par_js_fmt = par_js_fmt;
  this->fcalc = fcalc;

  return this;
}

char *fmodel_id (Fmodel *this) {
  return this->id;
}

char *fmodel_name (Fmodel *this) {
  return this->name;
}

Arr *fmodel_par_names (Fmodel *this) {
  return this->par_names;
}

Darr *fmodel_par_mins (Fmodel *this) {
  return this->par_mins;
}

Darr *fmodel_par_maxs (Fmodel *this) {
  return this->par_maxs;
}

// Arr[char]
Arr *fmodel_par_js_fmt (Fmodel *this) {
  return this->par_js_fmt;
}

Flea *fmodel_mk_flea (Fmodel *this, char *date, int cycle, int id) {
  double *mins = darr_start(this->par_mins);
  double *maxs = darr_start(this->par_maxs);
  int size = darr_size(this->par_mins);
  double *pars = ATOMIC(size * sizeof(double));
  double *ppars = pars;
  REPEAT(size) {
    double min = *mins++;
    *ppars++ = min + (*maxs++ - min) * rnd_d();
  }_REPEAT
  return flea_new(date, cycle, id, gen_new(size, pars));
}

Flea *fmodel_mutate_flea (
  Fmodel *this,
  Flea *flea,
  char *date,
  int cycle,
  int id
) {
  double *mins = darr_start(this->par_mins);
  double *maxs = darr_start(this->par_maxs);
  double *pars = gen_params(flea_gen(flea));
  int size = darr_size(this->par_mins);
  double *new_pars = ATOMIC(size * sizeof(double));
  double *ppars = new_pars;
  REPEAT(size) {
    double mul = MUTATION_MULTIPLIER * (rnd_d() * 2 - 1);
    double min = *mins++;
    double max = *maxs++;
    double old = *pars++;
    if (mul > 0)
      *ppars++ = old + (max - old) * mul;
    else
      *ppars++ = old + (old - min) * mul;
  }_REPEAT
  return flea_new(date, cycle, id, gen_new(size, pars));
}

void fmodel_calc (
  Fmodel *this,
  int n_cos,
  QtableRow *closes,
  Darr *params,
  void (*fn) (QtableRow closes, QtableRow refs)
) {
  this->fcalc(n_cos, closes, params, fn);
}

double *fmodel_refs (Fmodel *this, double *closes, Darr *params) {
  double *refs = ATOMIC(HISTORIC_QUOTES * sizeof(QtableRow));
  double *prefs = refs;

  void fn (QtableRow cs, QtableRow rs) {
    *prefs++ = *rs;
  }

  fmodel_calc(this, 1, qtable_from_column(closes), params, fn);

  return refs;
}

double fmodel_profits (
  Fmodel *this,
  double * opens,
  double *closes,
  Darr *params
) {
  int stocks = 0;
  double cash = BET;
  int to_sell = 1;
  int todo = 0;

  void fn (QtableRow cs, QtableRow rs) {
    double oq = *opens++;

    if (todo && oq > 0) {
      if (to_sell) { // there is buy order.
        stocks = (int)(cash / oq);
        cash -= broker_buy(stocks, oq);
      } else if (stocks > 0) {
        cash += broker_sell(stocks, oq);
        stocks = 0;
      }
      todo = 0;
    }

    double q = *cs;

    if (q < 0) return;

    double ref = *rs;
    if (to_sell) {
      if (ref > q) {
        todo = 1;
        to_sell = 0;
      }
    } else if (ref < q) {
      todo = 1;
      to_sell = 1;
    }
  }

  if (stocks > 0)
    cash += broker_sell(stocks, qtable_last_ok(closes));
  fmodel_calc(this, 1, qtable_from_column(closes), params, fn);

  return (cash - BET) / BET;
}

double fmodel_profits_avg (
  Fmodel *this,
  Qtable *opens,
  Qtable *closes,
  Darr *params
) {
  double **opens_vals = (double **)arr_start(qtable_cos_values(opens));
  double **closes_vals = (double **)arr_start(qtable_cos_values(closes));
  int len = arr_size(qtable_nicks(opens));
  double sum = 0;
  REPEAT(len) {
    sum += fmodel_profits(this, *opens_vals++, *closes_vals++, params);
  }_REPEAT
  return sum / len;
}

// Returns Arr[FmodelOrder]
Arr *fmodel_orders (
  Fmodel *this,
  Arr *dates, // Arr[char *]
  Qtable *opens,
  Qtable *closes,
  Darr *params
) {
  // Arr[FmodelOrder]
  Arr *orders = arr_new();

  char **dts = (char **)arr_start(dates);

  int n_cos = arr_size(qtable_nicks(opens));
  char *fn (Nick *nk) { return nick_name(nk); }
  // Arr[char]
  char **nicks = (char **)arr_start(arr_map(qtable_nicks(opens), (FCOPY)fn));
  QtableRow *ops = qtable_values(opens);
  QtableRow *cls = qtable_values(closes);

  double cash = INITIAL_CAPITAL;

  int stockss[n_cos];
  int *pstockss = stockss;
  int to_sells[n_cos];
  int *pto_sells = to_sells;
  REPEAT(n_cos) {
    *pstockss++ = 0;
    *pto_sells++ = 1;
  } _REPEAT

  // Arr[Dorder]
  Arr *dorders = arr_new();
  void fn2(QtableRow cs, QtableRow rs) {
    char *date = *dts++;
    QtableRow os = *ops++;

    if (arr_size(dorders)) {
      dorder_sort(dorders);
      EACH(dorders, Dorder, order) {
        int n_co = dorder_co_ix(order);
        double q = os[n_co];
        if (dorder_is_sell(order)) {
          int stocks = stockss[n_co];
          if (stocks > 0) {
            cash += broker_sell(stocks, q);
            stockss[n_co] = 0;
            arr_push(orders, fmodelOrder_new(
              date, nicks[n_co], 1, stocks, q
            ));
          }
          continue;
        }
        if (cash > MIN_TO_BET) {
          int stocks = (int)(BET / q);
          cash -= broker_buy(stocks, q);
          stockss[n_co] = stocks;
          arr_push(orders, fmodelOrder_new(
            date, nicks[n_co], 0, stocks, q
          ));
        }
      }_EACH
    }

    dorders = arr_new();
    RANGE0(i, n_cos) {
      double q = cs[i];

      if (q < 0) return;

      double ref = rs[i];
      if (to_sells[i]) {
        if (ref > q) {
          arr_push(dorders, dorder_sell(i));
          to_sells[i] = 0;
        }
      } else if (ref < q) {
        arr_push(dorders, dorder_buy(i, q / ref));
        to_sells[i] = 1;
      }
    }_RANGE
  }

  fmodel_calc(this, n_cos, cls, params, fn2);

  return orders;
}

Rs *fmodel_assets (
  Fmodel *this,
  Qtable *opens,
  Qtable *closes,
  Darr *params
) {
  int n_cos = arr_size(qtable_nicks(opens));
  QtableRow *ops = qtable_values(opens);
  QtableRow *cls = qtable_values(closes);

  double cash = INITIAL_CAPITAL;
  int buys = 0;
  int sells = 0;

  int stockss[n_cos];
  int *pstockss = stockss;
  int to_sells[n_cos];
  int *pto_sells = to_sells;
  REPEAT(n_cos) {
    *pstockss++ = 0;
    *pto_sells++ = 1;
  } _REPEAT

  // Arr[Dorder]
  Arr *dorders = arr_new();
  void fn2(QtableRow cs, QtableRow rs) {
    QtableRow os = *ops++;

    if (arr_size(dorders)) {
      dorder_sort(dorders);
      EACH(dorders, Dorder, order) {
        int n_co = dorder_co_ix(order);
        double q = os[n_co];
        if (dorder_is_sell(order)) {
          int stocks = stockss[n_co];
          if (stocks > 0) {
            cash += broker_sell(stocks, q);
            ++sells;
            stockss[n_co] = 0;
          }
          continue;
        }
        if (cash > MIN_TO_BET) {
          int stocks = (int)(BET / q);
          cash -= broker_buy(stocks, q);
          ++buys;
          stockss[n_co] = stocks;
        }
      }_EACH
    }

    dorders = arr_new();
    RANGE0(i, n_cos) {
      double q = cs[i];

      if (q < 0) return;

      double ref = rs[i];
      if (to_sells[i]) {
        if (ref > q) {
          arr_push(dorders, dorder_sell(i));
          to_sells[i] = 0;
        }
      } else if (ref < q) {
        arr_push(dorders, dorder_buy(i, q / ref));
        to_sells[i] = 1;
      }
    }_RANGE
  }

  fmodel_calc(this, n_cos, cls, params, fn2);

  RANGE0(i, n_cos) {
    int stocks = stockss[i];
    if (stocks > 0)
      cash += broker_sell(stocks, qtable_last_row_ok(cls, i));
  }_RANGE

  return rs_new(cash, buys, sells);
}

Js *fmodel_to_js (Fmodel *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(this->id));
  arr_push(js, js_ws(this->name));
  arr_push(js, arr_to_js(this->par_names, (FTO)js_ws));
  arr_push(js, darr_to_js(this->par_mins));
  arr_push(js, darr_to_js(this->par_maxs));
  arr_push(js, arr_to_js(this->par_js_fmt, (FTO)js_ws));
  return js_wa(js);
}
