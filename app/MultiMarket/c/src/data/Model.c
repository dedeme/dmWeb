// Copyright 11-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Model.h"
#include "dmc/Iarr.h"
#include "data/Facc.h"
#include "data/Nick.h"
#include "data/broker.h"
#include "data/Rank.h"
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

RsAssets *model_assets(
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
    facc_assets(acc, ncos, closes, HISTORIC_QUOTES - 1),
    nbuys,
    nsells
  );
}

// Returns Arr[RankAssets]
Arr *model_assets_historic(
  Model *this, Darr *params, Arr *dates, Qmatrix *opens, Qmatrix *closes
) {
  // Arr[RankAssets]
  Arr *r = arr_new();

  int ncos = arr_size(qmatrix_nicks(opens));
  char **dts = (char **)arr_start(dates);
  QmatrixValues *opensv = qmatrix_values(opens);
  QmatrixValues *closesv = qmatrix_values(closes);

  void **cos = arr_start(this->fcos(params, ncos, qmatrix_values(closes)));
  Facc *acc = facc_new(ncos);
  OrderCos *os = orderCos_new(ncos);
  int ix = 0;
  REPEAT(HISTORIC_QUOTES)
    char *date = *dts++;
    QmatrixValues ops = *opensv++;
    QmatrixValues cls = *closesv++;

    RANGE0(i, orderCos_nsells(os))
      int nco = orderCos_sells(os)[i];
      double price = ops[nco];
      if (price < 0) price = (*(closesv - 2))[nco];
      facc_sell(acc, nco, price);
    _REPEAT
    RANGE0(i, orderCos_nbuys(os))
      int nco = orderCos_buys(os)[i];
      double price = ops[nco];
      if (price < 0) price = (*(closesv - 2))[nco];
      if (!facc_buy(acc, nco, price)) break;
    _REPEAT

    os = orderCos_new(ncos);
    RANGE0(nco, ncos)
      orderCos_add(os, nco, this->forder(params, cos[nco], cls[nco]));
    _RANGE

    arr_push(r, rankAssets_new(date, facc_assets(acc, ncos, opens, ix++)));
  _REPEAT

  return r;
}

static double normalize(double min, double dif, double value) {
  return (value - min) / dif;
}

RsProfits *model_profits(
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

  return rsProfits_new(avg, mdv, avg * (avg >= 0 ? (1 - mdv) : (1 + mdv)));
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
