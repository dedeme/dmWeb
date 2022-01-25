// Copyright 16-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Model.h"
#include <math.h>
#include "dmc/err.h"
#include "dmc/js.h"
#include "dmc/ADouble.h"
#include "dmc/AInt.h"
#include "data/cts.h"
#include "data/broker.h"

Model *model_new (
  char *id, char *name, char *doc,
  Achar *param_names,
  double *param_bases, double *param_base_incs, double *param_env_incs,
  void (*calc)(
    AADouble *closes,
    double *params,
    void (*action)(ADouble *closes, ADouble *refs)
  )
){
  Model *this = MALLOC(Model);
  this->id = id;
  this->name = name;
  this->doc = doc;
  this->param_names = param_names;
  this->param_bases = param_bases;
  this->param_base_incs = param_base_incs;
  this->param_env_incs = param_env_incs;
  this->calc = calc;
  return this;
}

Result *model_result (Model *this, Quotes *qs, ADouble *params) {
  double bet = cts_bet();
  double min_to_bet = cts_min_to_bet();

  int ncos = achar_size(qs->cos);
  double a_cash = cts_initial_capital();

  AInt *a_stocks = aInt_new();
  AInt *p_stocks = aInt_new();
  ADouble *p_cashes = aDouble_new();
  AInt *operations = aInt_bf_new(ncos); // 0=Nop, 1=Buy, -1=Sell
  for (int i = 0; i < ncos; ++i) {
    aInt_push(a_stocks, 0);
    aInt_push(p_stocks, 0);
    aDouble_push(p_cashes, bet);
  }
  int sales = 0;

  int is_first = 1;
  ADouble *prev_refs = aDouble_new();
  ADouble *prev_closes = aDouble_new();
  ADouble **popens = qs->opens->es;

  /**/// CALLBACK

      void callback (ADouble *closes, ADouble *refs) {
        ADouble *opens = *popens++;
        if (is_first) {
          prev_refs = refs;
          prev_closes = closes;
          is_first = 0;
          return;
        }

        // Day beginning -----------------------------------

        int *poperations = operations->es;
        double *popens = opens ->es;
        int *pa_stocks = a_stocks->es;
        int *pp_stocks = p_stocks->es;
        double *pp_cashes = p_cashes->es;

        while (poperations < operations->end) {
          int op = *poperations++;
          double open = *popens++;
          int a_st = *pa_stocks;
          int p_st = *pp_stocks;
          double p_cash = *pp_cashes;

          if (op == 1) { // buy
            if (a_cash >= min_to_bet) {
              int stocks = bet / open;
              *pa_stocks = a_st + stocks;
              a_cash -= broker_buy(stocks, open);
            }

            int stocks = (p_cash - broker_fees(p_cash)) / open;
            double value = broker_buy(stocks, open);
            while (value > p_cash) {
              --stocks;
              value = broker_buy(stocks, open);
            }
            *pp_stocks = p_st + stocks;
            *pp_cashes = p_cash - value;
          } else if (op == -1) { // sell
            if (a_st > 0) {
              *pa_stocks = 0;
              a_cash += broker_sell(a_st, open);
              ++sales;
            }
            if (p_st > 0) {
              *pp_stocks = 0;
              *pp_cashes = p_cash + broker_sell(p_st, open);
            }
          }

          ++pa_stocks;
          ++pp_stocks;
          ++pp_cashes;
        }

        // Day end -----------------------------------------

        double *pcloses = closes->es;
        double *pprev_closes = prev_closes->es;
        double *prefs = refs->es;
        double *pprev_refs = prev_refs->es;
        operations = aInt_bf_new(ncos);
        while (pcloses < closes->end) {
          double close = *pcloses++;
          double prev_close = *pprev_closes++;
          double ref = *prefs++;
          double prev_ref = *pprev_refs++;
          if (close < ref) {
            if (prev_close >= prev_ref) { // Sell
              aInt_push(operations, -1);
            } else {
              aInt_push(operations, 0);
            }
          } else {
            if (prev_close < prev_ref) { // Buy
              aInt_push(operations, 1);
            } else {
              aInt_push(operations, 0);
            }
          }
        }

        prev_refs = refs;
        prev_closes = closes;
      }

  /**/// END CALLBACK

  this->calc(qs->closes, params->es, callback);

  ADouble *last_closes =
    aADouble_get(qs->closes, aADouble_size(qs->closes) - 1);
  double *plast_closes = last_closes->es;
  int *pa_stocks = a_stocks->es;
  int *pp_stocks = p_stocks->es;
  double *pp_cashes = p_cashes->es;
  double pf_sum = 0;
  while (plast_closes < last_closes->end) {
    double last_close = *plast_closes++;
    int a_st = *pa_stocks++;
    int p_st = *pp_stocks++;
    double p_cash = *pp_cashes++;

    if (a_st) a_cash += broker_sell(a_st, last_close);
    pf_sum += (p_cash + (p_st ? broker_sell(p_st, last_close) : 0) - bet) / bet;
  }

  return result_new(a_cash, pf_sum / ncos, sales);
}

Result *model_group_result(Model *this, Quotes *qs, ADouble *params) {
  int env_steps = cts_env_steps();
  int total_steps = env_steps * 2 + 1;

  Result *r = result_new(0, 0, 0);
  double *incs = this->param_env_incs;

  int nparams = aDouble_size(params);
  double *pparams = params->es;
  ADouble *pms = aDouble_new();
  int ixparams = 0;
  AInt *param_ixs = aInt_new();
  int results_n = 0;
  for (;;) {
    if (aInt_size(param_ixs) <= ixparams) {
      aInt_push(param_ixs, 0);
      double val = pparams[ixparams] + incs[ixparams] * (-env_steps);
      aDouble_push(pms, val);
    } else {
      int ixval = aInt_get(param_ixs, ixparams) + 1;
      if (ixval >= total_steps) {
        if (ixparams == 0) break;
        aInt_set(param_ixs, ixparams, -1);
        --ixparams;
        continue;
      }
      aInt_set(param_ixs, ixparams, ixval);
      double val = pparams[ixparams] +
        incs[ixparams] * (ixval - env_steps);
      aDouble_set(pms, ixparams, val);
    }
    ++ixparams;
    if (ixparams < nparams) continue;

    if (ixparams == nparams) {
      Result *rs = model_result(this, qs, pms);
      r = result_sum(r, rs);
      ++results_n;
    }

    --ixparams;
  }

  return result_div(r, results_n);
}

static AModelEval *range_evaluation (
  Model *this, Quotes *qs, AModelEval *evals, int is_new
) {
  int eval_steps = cts_eval_steps();

  AModelEval *r = aModelEval_new();

  double *bases = this->param_bases;
  double *incs = this->param_base_incs;

  int nparams = achar_size(this->param_names);
  ADouble *params = aDouble_new();
  int ixparams = 0;
  AInt *param_ixs = aInt_new();
  for (;;) {
    if (aInt_size(param_ixs) <= ixparams) {
      aInt_push(param_ixs, 0);
      aDouble_push(params, bases[ixparams]);
    } else {
      int ixval = aInt_get(param_ixs, ixparams) + 1;
      if (ixval >= eval_steps) {
        if (ixparams == 0) break;
        aInt_set(param_ixs, ixparams, -1);
        aDouble_set(params, ixparams, bases[ixparams] - incs[ixparams]);
        --ixparams;
        continue;
      }
      aInt_set(param_ixs, ixparams, ixval);
      double val = aDouble_get(params, ixparams) + incs[ixparams];
      aDouble_set(params, ixparams, val);
    }
    ++ixparams;
    if (ixparams < nparams) continue;

    if (ixparams == nparams) {

      /**/int ffind (ModelEval *e) {
      /**/  return aDouble_eq(params, e->params, 0.000001);
      /**/}
      ModelEval *mdev = oModelEval_osome(
        aModelEval_find(evals, ffind),
        modelEval_new(params, 0, 0, 0, 0, 0)
      );

      Result *rs = model_group_result(this, qs, params);

      int n = mdev->weeks;
      int n1 = n + 1;
      double value = result_eval(rs);
      double sales = rs->sales;
      ModelEval *new_mdev = is_new || n == 0
        ? modelEval_new(
            aDouble_copy(params),
            n < cts_eval_weeks() ? n1 : n,
            (mdev->hvalue * n + value) / n1,
            (mdev->hsales * n + sales) / n1,
            value,
            sales
          )
      : modelEval_new(
            aDouble_copy(params),
            n,
            (mdev->hvalue * n - mdev->value + value) / n,
            (mdev->hsales * n - mdev->sales + sales) / n,
            value,
            sales
          )
      ;

      aModelEval_push(r, new_mdev);
    }

    --ixparams;
  }

  return r;
}


AModelEval *model_range_new_evaluation (
  Model *this, Quotes *qs, AModelEval *evals
) {
  return range_evaluation(this, qs, evals, 1);
}

AModelEval *model_range_replace_evaluation (
  Model *this, Quotes *qs, AModelEval *evals
) {
  return range_evaluation(this, qs, evals, 0);
}

ADouble *model_refs(Model *this, Quotes *co_qs, ADouble *params) {
  ADouble *r = aDouble_new();
  void callback (ADouble *closes, ADouble *refs) {
    aDouble_push(r, *(refs->es));
  }
  this->calc(co_qs->closes, params->es, callback);
  return r;
}

ADouble *model_historic(Model *this, Quotes *qs, ADouble *params) {
  ADouble *r = aDouble_new();

  double bet = cts_bet();
  double min_to_bet = cts_min_to_bet();

  int ncos = achar_size(qs->cos);
  double a_cash = cts_initial_capital();

  ADouble *a_stocks = aDouble_new();
  AInt *operations = aInt_bf_new(ncos); // 0=Nop, 1=Buy, -1=Sell
  for (int i = 0; i < ncos; ++i) {
    aDouble_push(a_stocks, 0);
  }

  int is_first = 1;
  ADouble *prev_refs = aDouble_new();
  ADouble *prev_closes = aDouble_new();
  ADouble **popens = qs->opens->es;

  /**/// CALLBACK

      void callback (ADouble *closes, ADouble *refs) {
        ADouble *opens = *popens++;
        if (is_first) {
          prev_refs = refs;
          prev_closes = closes;
          is_first = 0;

          aDouble_push(r, a_cash);
          return;
        }

        // Day beginning -----------------------------------

        int *poperations = operations->es;
        double *popens = opens ->es;
        double *pa_stocks = a_stocks->es;

        while (poperations < operations->end) {
          int op = *poperations++;
          double open = *popens++;
          double a_st = *pa_stocks;

          if (op == 1) { // buy
            if (a_cash >= min_to_bet) {
              int stocks = bet / open;
              *pa_stocks = a_st + stocks;
              a_cash -= broker_buy(stocks, open);
            }
          } else if (op == -1) { // sell
            if (a_st > 0) {
              *pa_stocks = 0;
              a_cash += broker_sell(a_st, open);
            }
          }

          ++pa_stocks;
        }

        double *pcloses = closes->es;
        pa_stocks = a_stocks->es;
        double assets = a_cash;
        while (pcloses < closes->end) {
          double close = *pcloses++;
          double a_st = *pa_stocks++;
          assets += a_st * close;
        }
        aDouble_push(r, assets);

        // Day end -----------------------------------------

        pcloses = closes->es;
        double *pprev_closes = prev_closes->es;
        double *prefs = refs->es;
        double *pprev_refs = prev_refs->es;
        operations = aInt_bf_new(ncos);
        while (pcloses < closes->end) {
          double close = *pcloses++;
          double prev_close = *pprev_closes++;
          double ref = *prefs++;
          double prev_ref = *pprev_refs++;
          if (close < ref) {
            if (prev_close >= prev_ref) { // Sell
              aInt_push(operations, -1);
            } else {
              aInt_push(operations, 0);
            }
          } else {
            if (prev_close < prev_ref) { // Buy
              aInt_push(operations, 1);
            } else {
              aInt_push(operations, 0);
            }
          }
        }

        prev_refs = refs;
        prev_closes = closes;
      }

  /**/// END CALLBACK

  this->calc(qs->closes, params->es, callback);

  return r;
}

AOrder *model_orders(Model *this, Quotes *qs, ADouble *params) {
  AOrder *r = aOrder_new();

  double bet = cts_bet();
  double min_to_bet = cts_min_to_bet();

  int ncos = achar_size(qs->cos);
  double a_cash = cts_initial_capital();

  ADouble *a_stocks = aDouble_new();
  AInt *operations = aInt_bf_new(ncos); // 0=Nop, 1=Buy, -1=Sell
  for (int i = 0; i < ncos; ++i) {
    aDouble_push(a_stocks, 0);
  }

  int is_first = 1;
  ADouble *prev_refs = aDouble_new();
  ADouble *prev_closes = aDouble_new();
  ADouble **popens = qs->opens->es;
  char **pdates = qs->dates->es;

  /**/// CALLBACK

      void callback (ADouble *closes, ADouble *refs) {
        ADouble *opens = *popens++;
        char *date = *pdates++;
        if (is_first) {
          prev_refs = refs;
          prev_closes = closes;
          is_first = 0;
          return;
        }

        // Day beginning -----------------------------------

        int *poperations = operations->es;
        double *popens = opens ->es;
        double *pa_stocks = a_stocks->es;
        int ico = 0;
        while (poperations < operations->end) {
          int op = *poperations++;
          double open = *popens++;
          double a_st = *pa_stocks;

          if (op == 1) { // buy
            if (a_cash >= min_to_bet) {
              int stocks = bet / open;
              *pa_stocks = a_st + stocks;
              a_cash -= broker_buy(stocks, open);

              aOrder_push(r, order_new(
                date, qs->cos->es[ico], 0, stocks, open
              ));
            }
          } else if (op == -1) { // sell
            if (a_st > 0) {
              *pa_stocks = 0;
              a_cash += broker_sell(a_st, open);

              aOrder_push(r, order_new(
                date, qs->cos->es[ico], 1, a_st, open
              ));
            }
          }

          ++pa_stocks;
          ++ico;
        }

        double *pcloses = closes->es;
        pa_stocks = a_stocks->es;
        double assets = a_cash;
        while (pcloses < closes->end) {
          double close = *pcloses++;
          double a_st = *pa_stocks++;
          assets += a_st * close;
        }

        // Day end -----------------------------------------

        pcloses = closes->es;
        double *pprev_closes = prev_closes->es;
        double *prefs = refs->es;
        double *pprev_refs = prev_refs->es;
        operations = aInt_bf_new(ncos);
        while (pcloses < closes->end) {
          double close = *pcloses++;
          double prev_close = *pprev_closes++;
          double ref = *prefs++;
          double prev_ref = *pprev_refs++;
          if (close < ref) {
            if (prev_close >= prev_ref) { // Sell
              aInt_push(operations, -1);
            } else {
              aInt_push(operations, 0);
            }
          } else {
            if (prev_close < prev_ref) { // Buy
              aInt_push(operations, 1);
            } else {
              aInt_push(operations, 0);
            }
          }
        }

        prev_refs = refs;
        prev_closes = closes;
      }

  /**/// END CALLBACK

  this->calc(qs->closes, params->es, callback);

  return r;
}

/// Partial serialization.
char *model_to_js(Model *this) {
  int nparams = achar_size(this->param_names);
  return js_wa(achar_new_from(
    js_ws(this->id),
    js_ws(this->name),
    js_ws(this->doc),
    achar_to_js(this->param_names),
    aDouble_to_js(aDouble_new_c(nparams, this->param_bases)),
    aDouble_to_js(aDouble_new_c(nparams, this->param_base_incs)),
    aDouble_to_js(aDouble_new_c(nparams, this->param_env_incs)),
    NULL
  ));
}

Model *model_from_js(char *js) {
  return FAIL("Unimplemented");
}
