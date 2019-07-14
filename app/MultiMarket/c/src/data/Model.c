// Copyright 11-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Model.h"
#include "dmc/Iarr.h"
#include "data/Facc.h"
#include "data/Nick.h"
#include "data/broker.h"
#include "DEFS.h"

/* .
-Model
  # Model name.
  name: char *
  # Arr[ModelMxMn] Names of model parameters.
  param_cf: Arr - char
  # Template to show params in javascript (prefix-multiplicator-decimals-sufix)
  param_jss: Js
  -fparams: Fparams
  -fcos: Fcos
  -forder: Forder
  -fref: Fref
*/

struct Model_Model{
  char *name;
  Arr *param_cf;
  Js *param_jss;
  Fparams fparams;
  Fcos fcos;
  Forder forder;
  Fref fref;
};

Model *model_new(
  char *name,
  Arr *param_cf,
  Js *param_jss,
  Fparams fparams,
  Fcos fcos,
  Forder forder,
  Fref fref
) {
  Model *this = MALLOC(Model);
  this->name = name;
  this->param_cf = param_cf;
  this->param_jss = param_jss;
  this->fparams = fparams;
  this->fcos = fcos;
  this->forder = forder;
  this->fref = fref;
  return this;
}

char *model_name(Model *this) {
  return this->name;
}

Arr *model_param_cf(Model *this) {
  return this->param_cf;
}

Js *model_param_jss(Model *this) {
  return this->param_jss;
}

Darr *model_params(Model *this, Flea *f) {
  return this->fparams(f);
}

Arr *model_cos(Model *this, Darr *params, int qnicks, QmatrixValues *closes) {
  return this->fcos(params, qnicks, closes);
}

Order *model_order(Model *this, Darr *params, void *co, double q) {
  return this->forder(params, co, q);
}

double model_ref(Model *this, Darr *params, void *co) {
  return this->fref(params, co);
}

// set is Arr[char]
static Qmatrix *qset(Qmatrix *mx, Arr *set) {
  int set_contains(Nick *nk) {
    int fcontains (Nick *n) { return str_eq(nick_name(nk), nick_name(n)); }
    return it_contains(arr_to_it(set), (FPRED)fcontains);
  }

  Iarr *ixs = iarr_new();
  // Arr[Nick]
  Arr *nicks = arr_new();
  EACH_IX(qmatrix_nicks(mx), Nick, nk, ix)
    if (set_contains(nk)) {
      arr_push(nicks, nk);
      iarr_push(ixs, ix);
    }
  _EACH

  QmatrixValues *values = GC_MALLOC(HISTORIC_QUOTES * sizeof(QmatrixValues));
  QmatrixValues *mx_rows = qmatrix_values(mx);
  QmatrixValues *new_rows = values;
  REPEAT(HISTORIC_QUOTES)
    QmatrixValues vs = ATOMIC(iarr_size(ixs) * sizeof(double));
    IEACH_IX(ixs, nk_ix, i)
      vs[i] = (*mx_rows)[nk_ix];
    _EACH
    *new_rows++ = vs;
    ++mx_rows;
  _REPEAT

  return qmatrix_new(nicks, values);
}

static RsAssets *calculate_assets(
  Model *this, Flea *f, Qmatrix *opens, Qmatrix *closes
) {
  Darr *params = this->fparams(f);
  int ncos = arr_size(qmatrix_nicks(opens));
  QmatrixValues *opensv = qmatrix_values(opens);
  QmatrixValues *closesv = qmatrix_values(closes);

  void **cos = arr_start(this->fcos(params, ncos, qmatrix_values(closes)));
  Facc *acc = facc_new(ncos);
  int nbuys = 0;
  int nsells = 0;
  OrderCos *os = orderCos_new(ncos);
  REPEAT(HISTORIC_QUOTES)
    QmatrixValues ops = *opensv++;
    QmatrixValues cls = *closesv++;

    RANGE0(i, orderCos_nsells(os))
      int nco = orderCos_sells(os)[i];
      double price = ops[nco];
      if (price < 0) price = (*(closesv - 2))[nco];
      if (facc_sell(acc, nco, price)) ++nsells;
    _REPEAT
    RANGE0(i, orderCos_nbuys(os))
      int nco = orderCos_buys(os)[i];
      double price = ops[nco];
      if (price < 0) price = (*(closesv - 2))[nco];
      if (!facc_buy(acc, nco, price)) break;
      ++nbuys;
    _REPEAT

    os = orderCos_new(ncos);
    RANGE0(nco, ncos)
      orderCos_add(os, nco, this->forder(params, cos[nco], cls[nco]));
    _RANGE
  _REPEAT

  return rsAssets_new(
    facc_assets(acc, ncos, qmatrix_values(closes)[HISTORIC_QUOTES - 1]),
    nbuys,
    nsells
  );
}

RsAssets *model_assets(
  Model *this, Flea *f, NickSets *sets, Qmatrix *opens, Qmatrix *closes
) {
  RsAssets *calc(Arr *set) {
    return calculate_assets(this, f, qset(opens, set), qset(closes, set));
  }

  RsAssets *r1;
  void c1 (void *null) { r1 = calc(nickSets_win(sets)); }
  pthread_t *th1 = async_thread(c1, NULL);

  RsAssets *r2;
  void c2 (void *null) { r2 = calc(nickSets_loss(sets)); }
  pthread_t *th2 = async_thread(c2, NULL);

  RsAssets *r3;
  void c3 (void *null) { r3 = calc(nickSets_semi_win(sets)); }
  pthread_t *th3 = async_thread(c3, NULL);

  RsAssets *r4;
  void c4 (void *null) { r4 = calc(nickSets_semi_loss(sets)); }
  pthread_t *th4 = async_thread(c4, NULL);


  async_join(th1);
  async_join(th2);
  async_join(th3);
  async_join(th4);

  return rsAssets_new(
    ( rsAssets_assets(r1) +rsAssets_assets(r2) +
      rsAssets_assets(r3) + rsAssets_assets(r4)
    ) / 4.0,
    ( rsAssets_buys(r1) +rsAssets_buys(r2) +
      rsAssets_buys(r3) + rsAssets_buys(r4)
    ) / 4,
    ( rsAssets_sells(r1) +rsAssets_sells(r2) +
      rsAssets_sells(r3) + rsAssets_sells(r4)
    ) / 4
  );
}

static double normalize(double min, double dif, double value) {
  return (value - min) / dif;
}

static RsProfits *calculate_profits(
  Model *this, Flea *f, Qmatrix *opens, Qmatrix *closes
) {
  Darr *params = this->fparams(f);
  int ncos = arr_size(qmatrix_nicks(opens));
  // Arr[CO] (CO is company data of this)
  Arr *cos = this->fcos(params, ncos, qmatrix_values(closes));

  double sum = 0;
  double mx = -1000;
  double mn = 1000;
  Darr *ratios = darr_new();

  EACH_IX(cos, void, co, nco)
    QmatrixValues *opensv = qmatrix_values(opens);
    QmatrixValues *closesv = qmatrix_values(closes);
    double cash = 0;
    int stocks = 0;
    Order *order = order_none();
    double last_close = 0;
    REPEAT(HISTORIC_QUOTES)
      QmatrixValues ops = *opensv++;
      QmatrixValues cls = *closesv++;

      if (order_is_sell(order) && stocks) {
        double price = ops[nco];
        if (price < 0) price = (*(closesv - 2))[nco];
        cash += broker_sell(stocks, price);
        stocks = 0;
      } else if (order_is_buy(order)) {
        double price = ops[nco];
        if (price < 0) price = (*(closesv - 2))[nco];
        stocks = (int)(BET / price);
        cash -= broker_buy(stocks, price);
      }

      double price = cls[nco];
      order = this->forder(params, co, price);
      if (price > 0) last_close = price;
    _REPEAT

    if (stocks) cash += broker_sell(stocks, last_close);

    double ratio = cash / BET;
    darr_push(ratios, ratio);
    sum += ratio;
    if (ratio > mx) mx = ratio;
    if (ratio < mn) mn = ratio;
  _EACH

  int cos_size = arr_size(cos);
  double avg = sum / cos_size;
  double dif = mx - mn;

  double avg_n = normalize(mn, dif, avg);

  double sum_n = 0;
  DEACH(ratios, r)
    double d = normalize(mn, dif, r) - avg_n;
    if (d < 0) sum_n -= d;
    else sum_n += d;
  _EACH
  double mdv = sum_n / cos_size;

  return rsProfits_new(avg, mdv, avg * (1 - mdv));
}

RsProfits *model_profits(
  Model *this, Flea *f, NickSets *sets, Qmatrix *opens, Qmatrix *closes
) {
  RsProfits *calc(Arr *set) {
    return calculate_profits(this, f, qset(opens, set), qset(closes, set));
  }

  RsProfits *r1;
  void c1 (void *null) { r1 = calc(nickSets_win(sets)); }
  pthread_t *th1 = async_thread(c1, NULL);

  RsProfits *r2;
  void c2 (void *null) { r2 = calc(nickSets_loss(sets)); }
  pthread_t *th2 = async_thread(c2, NULL);

  RsProfits *r3;
  void c3 (void *null) { r3 = calc(nickSets_semi_win(sets)); }
  pthread_t *th3 = async_thread(c3, NULL);

  RsProfits *r4;
  void c4 (void *null) { r4 = calc(nickSets_semi_loss(sets)); }
  pthread_t *th4 = async_thread(c4, NULL);


  async_join(th1);
  async_join(th2);
  async_join(th3);
  async_join(th4);

  return rsProfits_new(
    ( rsProfits_avg(r1) +rsProfits_avg(r2) +
      rsProfits_avg(r3) + rsProfits_avg(r4)
    ) / 4.0,
    ( rsProfits_var(r1) +rsProfits_var(r2) +
      rsProfits_var(r3) + rsProfits_var(r4)
    ) / 4.0,
    ( rsProfits_sel(r1) +rsProfits_sel(r2) +
      rsProfits_sel(r3) + rsProfits_sel(r4)
    ) / 4.0
  );
}

RsCharts *model_charts(
  Model *this, Flea *f, Arr *dates, Qmatrix *opens, Qmatrix *closes
) {
  Darr *params = this->fparams(f);
  // Arr[Nick]
  Arr *nicks = qmatrix_nicks(opens);
  int ncos = arr_size(nicks);
  // Arr[CO] (CO is company data of this)
  Arr *cos = this->fcos(params, ncos, qmatrix_values(closes));

  // Arr[RsChart]
  Arr *rss = arr_new();
  EACH_IX(cos, void, co, nco)
    char *name = nick_name(arr_get(nicks, nco));
    char **dts = (char **)arr_start(dates);
    QmatrixValues *opensv = qmatrix_values(opens);
    QmatrixValues *closesv = qmatrix_values(closes);


    // Arr[RsCharQ]
    Arr *rsq = arr_new();
    // Arr[RsCharOp]
    Arr *rsop = arr_new();
    double cash = 0;
    int stocks = 0;
    Order *order = order_none();
    double last_close = 0;
    REPEAT(HISTORIC_QUOTES)
      char *date = *dts++;
      QmatrixValues ops = *opensv++;
      QmatrixValues cls = *closesv++;


      if (order_is_sell(order) && stocks) {
        double price = ops[nco];
        if (price < 0) price = (*(closesv - 2))[nco];

        arr_push(rsop, rsChartOp_new(1, date, stocks, price));

        cash += broker_sell(stocks, price);
        stocks = 0;
      } else if (order_is_buy(order)) {
        double price = ops[nco];
        if (price < 0) price = (*(closesv - 2))[nco];
        stocks = (int)(BET / price);

        arr_push(rsop, rsChartOp_new(0, date, stocks, price));

        cash -= broker_buy(stocks, price);
      }

      double price = cls[nco];

      order = this->forder(params, co, price);
      if (price > 0) {
        arr_push(rsq, rsChartQ_new(date, price, this->fref(params, co)));
        last_close = price;
      }
    _REPEAT

    if (stocks) cash += broker_sell(stocks, last_close);

    double ratio = cash / BET;

    arr_push(rss, rsChart_new(name, ratio, rsq, rsop));
  _EACH

  return rsCharts_new(rss);
}

RsHistoric *model_historic (
  Model *this, Darr *params, Arr *dates, Darr *opens, Darr *closes
) {
  int qsize = arr_size(dates);

  QmatrixValues *rows = GC_MALLOC(qsize * sizeof(QmatrixValues));
  QmatrixValues *p = rows;
  double *cls = darr_start(closes);
  REPEAT(qsize)
    *p = ATOMIC(sizeof(double));
    **p++ = *cls++;
  _REPEAT
  void *co = *(arr_start(this->fcos(params, 1, rows)));

  char **dts = (char **)arr_start(dates);
  double *ops = darr_start(opens);
  cls = darr_start(closes);

  // Arr[RsCharQ]
  Arr *rsq = arr_new();
  // Arr[RsCharOp]
  Arr *rsop = arr_new();

  double cash = 0;
  int stocks = 0;
  Order *order = order_none();
  double last_close = 0;
  REPEAT(qsize) {
    if (order_is_sell(order) && stocks) {
      double price = *ops;
      if (price < 0) price = *(cls - 1);

      arr_push(rsop, rsChartOp_new(1, *dts, stocks, price));

      cash += broker_sell(stocks, price);
      stocks = 0;
    } else if (order_is_buy(order)) {
      double price = *ops;
      if (price < 0) price = *(cls - 1);
      stocks = (int)(BET / price);

      arr_push(rsop, rsChartOp_new(0, *dts, stocks, price));

      cash -= broker_buy(stocks, price);
    }

    double price = *cls;

    order = this->forder(params, co, price);

    if (price > 0)  {
      arr_push(rsq, rsChartQ_new(*dts, price, this->fref(params, co)));
      last_close = price;
    }

    ++dts;
    ++ops;
    ++cls;
  }_REPEAT

  if (stocks) cash += broker_sell(stocks, last_close);
  double ratio = cash / BET;

  return rsHistoric_new(
    co, ratio, rsq, rsop, order, this->fref(params, co), stocks
  );
}
