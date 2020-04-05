// Copyright 24-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "chart/summary.h"
#include "db/quotes.h"
#include "data/Qtable.h"
#include "data/broker.h"
#include "data/flea/Dorder.h"
#include "DEFS.h"

/* .
# Record for historic data.
-SummaryData: to
  date   : char *
  assets : double
  profits: double
  market : double
*/

/*--*/

struct summary_SummaryData {
  char *date;
  double assets;
  double profits;
  double market;
};

static SummaryData *_summaryData_new (
  char *date,
  double assets,
  double profits,
  double market
) {
  SummaryData *this = MALLOC(SummaryData);
  this->date = date;
  this->assets = assets;
  this->profits = profits;
  this->market = market;
  return this;
}

char *summaryData_date (SummaryData *this) {
  return this->date;
}

double summaryData_assets (SummaryData *this) {
  return this->assets;
}

double summaryData_profits (SummaryData *this) {
  return this->profits;
}

double summaryData_market (SummaryData *this) {
  return this->market;
}

Js *summaryData_to_js (SummaryData *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(this->date));
  arr_push(js, js_wd(this->assets));
  arr_push(js, js_wd(this->profits));
  arr_push(js, js_wd(this->market));
  return js_wa(js);
}

/*--*/

// Returns Arr[SumaryData]
Arr *summary_historic(
  Fmodel *model,
  Arr *dates, // Arr[char]
  Qtable *opens_table,
  Qtable *closes_table,
  Darr *params
) {
  // Arr[SummaryData]
  Arr *r = arr_new();

  int ncos = arr_size(qtable_nicks(opens_table));
  char **dts = (char **)arr_start(dates);
  QtableRow *opens = qtable_values(opens_table);
  QtableRow *closes = qtable_values(closes_table);

  double pf_cash = INITIAL_CAPITAL;
  int pf_stockss[ncos];
  int *ppf_stockss = pf_stockss;

  double cashes[ncos];
  double *pcashes = cashes;
  int all_stockss[ncos];
  int *pall_stockss = all_stockss;

  double *market = *closes;

  int to_sells[ncos];
  int *pto_sells = to_sells;
  REPEAT(ncos) {
    *ppf_stockss++ = 0;

    *pcashes++ = BET;
    *pall_stockss++ = 0;

    *pto_sells++ = 1;
  } _REPEAT

  // Arr[Dorder]
  Arr *dorders = arr_new();
  void fn (QtableRow cls, QtableRow refs) {
    char *date = *dts++;
    QtableRow ops = *opens++;


    if (arr_size(dorders)) {
      dorder_sort(dorders);
      EACH(dorders, Dorder, order) {
        int nco = dorder_co_ix(order);
        double q = ops[nco];
        if (dorder_is_sell(order)) {
          int stocks = pf_stockss[nco];
          if (stocks > 0) {
            pf_cash += broker_sell(stocks, q);
            pf_stockss[nco] = 0;
          }

          stocks = all_stockss[nco];
          if (stocks > 0) {
            cashes[nco] += broker_sell(stocks, q);
            all_stockss[nco] = 0;
          }
          continue;
        }
        int stocks = (int)(BET / q);
        if (pf_cash > MIN_TO_BET) {
          pf_cash -= broker_buy(stocks, q);
          pf_stockss[nco] = stocks;
        }

        cashes[nco] -= broker_buy(stocks, q);
        all_stockss[nco] = stocks;
      }_EACH
    }

    dorders = arr_new();
    RANGE0(i, ncos) {
      double q = cls[i];

      if (q < 0) return;

      double ref = refs[i];
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

    double pf_sum = pf_cash;
    double sum = 0;
    double market_df = 0;
    RANGE0(i, ncos) {
      double q = cls[i];
      sum += cashes[i] + all_stockss[i] * q;
      pf_sum += pf_stockss[i] * q;
      double q0 = market[i];
      market_df += (q - q0) / q0;
    }_RANGE
    double inv = BET * ncos;
    arr_push(r, _summaryData_new(
      date, pf_sum, (sum - inv) / inv, market_df / ncos
    ));
  }
  fmodel_calc(model, ncos, closes, params, fn);

  return r;
}

Iarr *summary_ranking(
  Fmodel *model,
  Flea *flea,
  Arr *ranking // Arr[Arr[Investor]]
);
