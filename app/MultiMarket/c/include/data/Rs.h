// Copyright 13-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Result records.

#ifndef DATA_RS_H
  #define DATA_RS_H

#include "dmc/std.h"
#include "Flea.h"
#include "Order.h"

/*--*/

///
typedef struct Rs_RsAssets RsAssets;

///
RsAssets *rsAssets_new(double assets, int buys, int sells);

///
double rsAssets_assets(RsAssets *this);

///
int rsAssets_buys(RsAssets *this);

///
int rsAssets_sells(RsAssets *this);

///
Js *rsAssets_to_js(RsAssets *this);

///
RsAssets *rsAssets_from_js(Js *js);

///
typedef struct Rs_RsProfits RsProfits;

///
RsProfits *rsProfits_new(double avg, double var, double sel);

///
double rsProfits_avg(RsProfits *this);

///
double rsProfits_var(RsProfits *this);

///
double rsProfits_sel(RsProfits *this);

///
Js *rsProfits_to_js(RsProfits *this);

///
RsProfits *rsProfits_from_js(Js *js);

///
typedef struct Rs_Rs Rs;

///
Rs *rs_new(Flea *flea, RsAssets *assets, RsProfits *profits);

///
Flea *rs_flea(Rs *this);

///
RsAssets *rs_assets(Rs *this);

///
RsProfits *rs_profits(Rs *this);

///
Js *rs_to_js(Rs *this);

///
Rs *rs_from_js(Js *js);

///
typedef struct Rs_RsWeb RsWeb;

///
RsWeb *rsWeb_new(Rs *result, Darr *params);

///
Rs *rsWeb_result(RsWeb *this);

///
Darr *rsWeb_params(RsWeb *this);

///
Js *rsWeb_to_js(RsWeb *this);

///
RsWeb *rsWeb_from_js(Js *js);

/// Bests fleas result
typedef struct Rs_RsBests RsBests;

///
RsBests *rsBests_new(char *date, RsWeb *result);

///
char *rsBests_date(RsBests *this);

///
RsWeb *rsBests_result(RsBests *this);

///
Js *rsBests_to_js(RsBests *this);

///
RsBests *rsBests_from_js(Js *js);

/// Champion fleas result
typedef struct Rs_RsChampions RsChampions;

///
RsChampions *rsChampions_new(char *model, RsWeb *result);

///
char *rsChampions_model(RsChampions *this);

///
RsWeb *rsChampions_result(RsChampions *this);

///
Js *rsChampions_to_js(RsChampions *this);

///
RsChampions *rsChampions_from_js(Js *js);

///
typedef struct Rs_RsChartQ RsChartQ;

///
RsChartQ *rsChartQ_new(char *date, double close, double ref);

///
char *rsChartQ_date(RsChartQ *this);

///
double rsChartQ_close(RsChartQ *this);

///
double rsChartQ_ref(RsChartQ *this);

///
Js *rsChartQ_to_js(RsChartQ *this);

///
RsChartQ *rsChartQ_from_js(Js *js);

///
typedef struct Rs_RsChartOp RsChartOp;

///
RsChartOp *rsChartOp_new(
  int is_sell,
  char *date,
  int stocks,
  double price
);

///
int rsChartOp_is_sell(RsChartOp *this);

///
char *rsChartOp_date(RsChartOp *this);

///
int rsChartOp_stocks(RsChartOp *this);

///
double rsChartOp_price(RsChartOp *this);

///
Js *rsChartOp_to_js(RsChartOp *this);

///
RsChartOp *rsChartOp_from_js(Js *js);

///
typedef struct Rs_RsChart RsChart;

///
RsChart *rsChart_new(
  char *nick,
  double profits,
  Arr *quotes,
  Arr *historic
);

///
char *rsChart_nick(RsChart *this);

///
double rsChart_profits(RsChart *this);

/// Arr[RsChartQ] Dates and quotes
Arr *rsChart_quotes(RsChart *this);

/// Arr[RsChartOp] Operations
Arr *rsChart_historic(RsChart *this);

///
Js *rsChart_to_js(RsChart *this);

///
RsChart *rsChart_from_js(Js *js);

/// Charts fleas results
typedef struct Rs_RsCharts RsCharts;

///
RsCharts *rsCharts_new(Arr *cos);

/// Arr[RsChart]
Arr *rsCharts_cos(RsCharts *this);

/// Accounting results of one company
typedef struct Rs_RsHistoric RsHistoric;

///
RsHistoric *rsHistoric_new(
  void *co,
  double profits,
  Arr *quotes,
  Arr *historic,
  Order *order,
  double ref,
  int stocks
);

/// Company data
void *rsHistoric_co(RsHistoric *this);

///
double rsHistoric_profits(RsHistoric *this);

/// Arr[RsChartQ] Dates and quotes
Arr *rsHistoric_quotes(RsHistoric *this);

/// Arr[RsChartOp] Operations
Arr *rsHistoric_historic(RsHistoric *this);

/// Last order
Order *rsHistoric_order(RsHistoric *this);

/// Last ref
double rsHistoric_ref(RsHistoric *this);

/// stocks in portfolio
int rsHistoric_stocks(RsHistoric *this);

/*--*/

/// Returns 'bests' without duplicates filtering with flea (date + cycle + id)
///   bests: Arr[RsBests]
///   return: Arr[RsBests] without duplicates.
Arr *rsBests_distinct (Arr *bests);

#endif
