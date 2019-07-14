// Copyright 13-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Rs.h"

/* .
RsAssets: serial
  assets: double
  buys: int
  sells: int
===
RsProfits: serial
  avg: double
  var: double
  sel: double
===
Rs: serial
  flea: Flea
  assets: RsAssets
  profits: RsProfits
===
RsWeb: serial
  result: Rs
  params: Darr
===
# Bests fleas result
RsBests: serial
  date: char *
  result: RsWeb
===
# Champion fleas result
RsChampions: serial
  model: char *
  result: RsWeb
===
RsChartQ: serial
  date: char *
  close: double
  ref: double
===
RsChartOp: serial
  is_sell: bool
  date: char *
  stocks: int
  price: double
===
RsChart: serial
  nick: char *
  profits: double
  # Arr[RsChartQ] Dates and quotes
  quotes: Arr - RsChartQ
  # Arr[RsChartOp] Operations
  historic: Arr - RsChartOp
===
# Charts fleas results
RsCharts
  # Arr[RsChart]
  cos: Arr - RsChart

===

# Accounting results of one company
RsHistoric
  # Company data
  co: void
  profits: double
  # Arr[RsChartQ] Dates and quotes
  quotes: Arr - RsChartQ
  # Arr[RsChartOp] Operations
  historic: Arr - RsChartOp
  # Last order
  order: Order
  # Last ref
  ref: double
  # stocks in portfolio
  stocks: int
*/

/*--*/

struct Rs_RsAssets {
  double assets;
  int buys;
  int sells;
};

RsAssets *rsAssets_new (double assets, int buys, int sells) {
  RsAssets *this = MALLOC(RsAssets);
  this->assets = assets;
  this->buys = buys;
  this->sells = sells;
  return this;
}

double rsAssets_assets (RsAssets *this) {
  return this->assets;
}

int rsAssets_buys (RsAssets *this) {
  return this->buys;
}

int rsAssets_sells (RsAssets *this) {
  return this->sells;
}

Js *rsAssets_to_js (RsAssets *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_wd(this->assets));
  arr_push(js, js_wi((int)this->buys));
  arr_push(js, js_wi((int)this->sells));
  return js_wa(js);
}

RsAssets *rsAssets_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  RsAssets *this = MALLOC(RsAssets);
  this->assets = js_rd(*p++);
  this->buys = js_ri(*p++);
  this->sells = js_ri(*p++);
  return this;
}

struct Rs_RsProfits {
  double avg;
  double var;
  double sel;
};

RsProfits *rsProfits_new (double avg, double var, double sel) {
  RsProfits *this = MALLOC(RsProfits);
  this->avg = avg;
  this->var = var;
  this->sel = sel;
  return this;
}

double rsProfits_avg (RsProfits *this) {
  return this->avg;
}

double rsProfits_var (RsProfits *this) {
  return this->var;
}

double rsProfits_sel (RsProfits *this) {
  return this->sel;
}

Js *rsProfits_to_js (RsProfits *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_wd(this->avg));
  arr_push(js, js_wd(this->var));
  arr_push(js, js_wd(this->sel));
  return js_wa(js);
}

RsProfits *rsProfits_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  RsProfits *this = MALLOC(RsProfits);
  this->avg = js_rd(*p++);
  this->var = js_rd(*p++);
  this->sel = js_rd(*p++);
  return this;
}

struct Rs_Rs {
  Flea *flea;
  RsAssets *assets;
  RsProfits *profits;
};

Rs *rs_new (Flea *flea, RsAssets *assets, RsProfits *profits) {
  Rs *this = MALLOC(Rs);
  this->flea = flea;
  this->assets = assets;
  this->profits = profits;
  return this;
}

Flea *rs_flea (Rs *this) {
  return this->flea;
}

RsAssets *rs_assets (Rs *this) {
  return this->assets;
}

RsProfits *rs_profits (Rs *this) {
  return this->profits;
}

Js *rs_to_js (Rs *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, flea_to_js(this->flea));
  arr_push(js, rsAssets_to_js(this->assets));
  arr_push(js, rsProfits_to_js(this->profits));
  return js_wa(js);
}

Rs *rs_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  Rs *this = MALLOC(Rs);
  this->flea = flea_from_js(*p++);
  this->assets = rsAssets_from_js(*p++);
  this->profits = rsProfits_from_js(*p++);
  return this;
}

struct Rs_RsWeb {
  Rs *result;
  Darr *params;
};

RsWeb *rsWeb_new (Rs *result, Darr *params) {
  RsWeb *this = MALLOC(RsWeb);
  this->result = result;
  this->params = params;
  return this;
}

Rs *rsWeb_result (RsWeb *this) {
  return this->result;
}

Darr *rsWeb_params (RsWeb *this) {
  return this->params;
}

Js *rsWeb_to_js (RsWeb *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, rs_to_js(this->result));
  arr_push(js, darr_to_js(this->params));
  return js_wa(js);
}

RsWeb *rsWeb_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  RsWeb *this = MALLOC(RsWeb);
  this->result = rs_from_js(*p++);
  this->params = darr_from_js(*p++);
  return this;
}

struct Rs_RsBests {
  char *date;
  RsWeb *result;
};

RsBests *rsBests_new (char *date, RsWeb *result) {
  RsBests *this = MALLOC(RsBests);
  this->date = date;
  this->result = result;
  return this;
}

char *rsBests_date (RsBests *this) {
  return this->date;
}

RsWeb *rsBests_result (RsBests *this) {
  return this->result;
}

Js *rsBests_to_js (RsBests *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(this->date));
  arr_push(js, rsWeb_to_js(this->result));
  return js_wa(js);
}

RsBests *rsBests_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  RsBests *this = MALLOC(RsBests);
  this->date = js_rs(*p++);
  this->result = rsWeb_from_js(*p++);
  return this;
}

struct Rs_RsChampions {
  char *model;
  RsWeb *result;
};

RsChampions *rsChampions_new (char *model, RsWeb *result) {
  RsChampions *this = MALLOC(RsChampions);
  this->model = model;
  this->result = result;
  return this;
}

char *rsChampions_model (RsChampions *this) {
  return this->model;
}

RsWeb *rsChampions_result (RsChampions *this) {
  return this->result;
}

Js *rsChampions_to_js (RsChampions *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(this->model));
  arr_push(js, rsWeb_to_js(this->result));
  return js_wa(js);
}

RsChampions *rsChampions_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  RsChampions *this = MALLOC(RsChampions);
  this->model = js_rs(*p++);
  this->result = rsWeb_from_js(*p++);
  return this;
}

struct Rs_RsChartQ {
  char *date;
  double close;
  double ref;
};

RsChartQ *rsChartQ_new (char *date, double close, double ref) {
  RsChartQ *this = MALLOC(RsChartQ);
  this->date = date;
  this->close = close;
  this->ref = ref;
  return this;
}

char *rsChartQ_date (RsChartQ *this) {
  return this->date;
}

double rsChartQ_close (RsChartQ *this) {
  return this->close;
}

double rsChartQ_ref (RsChartQ *this) {
  return this->ref;
}

Js *rsChartQ_to_js (RsChartQ *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(this->date));
  arr_push(js, js_wd(this->close));
  arr_push(js, js_wd(this->ref));
  return js_wa(js);
}

RsChartQ *rsChartQ_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  RsChartQ *this = MALLOC(RsChartQ);
  this->date = js_rs(*p++);
  this->close = js_rd(*p++);
  this->ref = js_rd(*p++);
  return this;
}

struct Rs_RsChartOp {
  int is_sell;
  char *date;
  int stocks;
  double price;
};

RsChartOp *rsChartOp_new (
  int is_sell,
  char *date,
  int stocks,
  double price
) {
  RsChartOp *this = MALLOC(RsChartOp);
  this->is_sell = is_sell;
  this->date = date;
  this->stocks = stocks;
  this->price = price;
  return this;
}

int rsChartOp_is_sell (RsChartOp *this) {
  return this->is_sell;
}

char *rsChartOp_date (RsChartOp *this) {
  return this->date;
}

int rsChartOp_stocks (RsChartOp *this) {
  return this->stocks;
}

double rsChartOp_price (RsChartOp *this) {
  return this->price;
}

Js *rsChartOp_to_js (RsChartOp *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_wb(this->is_sell));
  arr_push(js, js_ws(this->date));
  arr_push(js, js_wi((int)this->stocks));
  arr_push(js, js_wd(this->price));
  return js_wa(js);
}

RsChartOp *rsChartOp_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  RsChartOp *this = MALLOC(RsChartOp);
  this->is_sell = js_rb(*p++);
  this->date = js_rs(*p++);
  this->stocks = js_ri(*p++);
  this->price = js_rd(*p++);
  return this;
}

struct Rs_RsChart {
  char *nick;
  double profits;
  Arr *quotes;
  Arr *historic;
};

RsChart *rsChart_new (
  char *nick,
  double profits,
  Arr *quotes,
  Arr *historic
) {
  RsChart *this = MALLOC(RsChart);
  this->nick = nick;
  this->profits = profits;
  this->quotes = quotes;
  this->historic = historic;
  return this;
}

char *rsChart_nick (RsChart *this) {
  return this->nick;
}

double rsChart_profits (RsChart *this) {
  return this->profits;
}

Arr *rsChart_quotes (RsChart *this) {
  return this->quotes;
}

Arr *rsChart_historic (RsChart *this) {
  return this->historic;
}

Js *rsChart_to_js (RsChart *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(this->nick));
  arr_push(js, js_wd(this->profits));
  arr_push(js, arr_to_js(this->quotes, (FTO)rsChartQ_to_js));
  arr_push(js, arr_to_js(this->historic, (FTO)rsChartOp_to_js));
  return js_wa(js);
}

RsChart *rsChart_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  RsChart *this = MALLOC(RsChart);
  this->nick = js_rs(*p++);
  this->profits = js_rd(*p++);
  this->quotes = arr_from_js(*p++, (FFROM)rsChartQ_from_js);
  this->historic = arr_from_js(*p++, (FFROM)rsChartOp_from_js);
  return this;
}

struct Rs_RsCharts {
  Arr *cos;
};

RsCharts *rsCharts_new (Arr *cos) {
  RsCharts *this = MALLOC(RsCharts);
  this->cos = cos;
  return this;
}

Arr *rsCharts_cos (RsCharts *this) {
  return this->cos;
}

struct Rs_RsHistoric {
  void *co;
  double profits;
  Arr *quotes;
  Arr *historic;
  Order *order;
  double ref;
  int stocks;
};

RsHistoric *rsHistoric_new (
  void *co,
  double profits,
  Arr *quotes,
  Arr *historic,
  Order *order,
  double ref,
  int stocks
) {
  RsHistoric *this = MALLOC(RsHistoric);
  this->co = co;
  this->profits = profits;
  this->quotes = quotes;
  this->historic = historic;
  this->order = order;
  this->ref = ref;
  this->stocks = stocks;
  return this;
}

void *rsHistoric_co (RsHistoric *this) {
  return this->co;
}

double rsHistoric_profits (RsHistoric *this) {
  return this->profits;
}

Arr *rsHistoric_quotes (RsHistoric *this) {
  return this->quotes;
}

Arr *rsHistoric_historic (RsHistoric *this) {
  return this->historic;
}

Order *rsHistoric_order (RsHistoric *this) {
  return this->order;
}

double rsHistoric_ref (RsHistoric *this) {
  return this->ref;
}

int rsHistoric_stocks (RsHistoric *this) {
  return this->stocks;
}

/*--*/

Arr *rsBests_distinct (Arr *bests) {
  // Arr[RsBests]
  Arr *r = arr_new();
  EACH(bests, RsBests, rs)
    Flea *f1 = ((rs->result)->result)->flea;
    int ok = 1;
    EACH(r, RsBests, rs2)
      Flea *f2 = ((rs2->result)->result)->flea;
      if (
        str_eq(flea_date(f1), flea_date(f2)) &&
        flea_cycle(f1) == flea_cycle(f2) &&
        flea_id(f1) == flea_id(f2)
      ) {
        ok = 0;
        break;
      }
    _EACH
    if (ok) {
      arr_push(r, rs);
    }
  _EACH
  return r;
}
