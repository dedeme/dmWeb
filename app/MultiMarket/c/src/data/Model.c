// Copyright 11-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Model.h"
#include "data/Facc.h"
#include "data/Nick.h"
#include "broker.h"
#include "DEFS.h"

/* .
-Model
  # Model name.
  name: char *
  # Arr[char] Names of model parameters.
  param_names: Arr - char
  # Template to show params in javascript (prefix-multiplicator-decimals-sufix)
  param_jss: Js
  -fparams: Fparams
  -fcos: Fcos
  -forder: Forder
  -fref: Fref
*/

struct Model_Model{
  char *name;
  Arr *param_names;
  Js *param_jss;
  Fparams fparams;
  Fcos fcos;
  Forder forder;
  Fref fref;
};

Model *model_new(
  char *name,
  Arr *param_names,
  Js *param_jss,
  Fparams fparams,
  Fcos fcos,
  Forder forder,
  Fref fref
) {
  Model *this = MALLOC(Model);
  this->name = name;
  this->param_names = param_names;
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

Arr *model_param_names(Model *this) {
  return this->param_names;
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

RsAssets *model_assets(Model *this, Flea *f, Qmatrix *opens, Qmatrix *closes) {
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

  return rsProfits_new(avg, mdv, avg * (1 - mdv));
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

      if (price > 0) {
        arr_push(rsq, rsChartQ_new(date, price, this->fref(params, co)));
      }

      order = this->forder(params, co, price);
      if (price > 0) last_close = price;
    _REPEAT

    if (stocks) cash += broker_sell(stocks, last_close);

    double ratio = cash / BET;

    arr_push(rss, rsChart_new(name, ratio, rsq, rsop));
  _EACH

  return rsCharts_new(rss);
}
