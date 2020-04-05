// Copyright 19-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "scheduler/fleas.h"
#include "dmc/date.h"
#include "data/flea/fmodels.h"
#include "data/flea/Investor.h"
#include "db/quotes.h"
#include "db/fleas/flog.h"
#include "db/fleas/fmodels.h"
#include "db/fleas/ranking.h"
#include "db/log.h"
#include "DEFS.h"

/* .
# Flea evaluation data.
-FleasEval: serial
  # Evaluated flea.
  flea: Flea
  # Number of buys.
  buys: int
  # Number of sells.
  sells: int
  # Assets in simulation (money).
  assets: double
  # Profits average of every company (ratios --can be < 0).
  profits: double
  # Evalutarion result. Its value is between -1 and 1.
  eval: double
*/

/*--*/

struct fleas_FleasEval {
  Flea *flea;
  int buys;
  int sells;
  double assets;
  double profits;
  double eval;
};

static FleasEval *_fleasEval_new (
  Flea *flea,
  int buys,
  int sells,
  double assets,
  double profits,
  double eval
) {
  FleasEval *this = MALLOC(FleasEval);
  this->flea = flea;
  this->buys = buys;
  this->sells = sells;
  this->assets = assets;
  this->profits = profits;
  this->eval = eval;
  return this;
}

Flea *fleasEval_flea (FleasEval *this) {
  return this->flea;
}

int fleasEval_buys (FleasEval *this) {
  return this->buys;
}

int fleasEval_sells (FleasEval *this) {
  return this->sells;
}

double fleasEval_assets (FleasEval *this) {
  return this->assets;
}

double fleasEval_profits (FleasEval *this) {
  return this->profits;
}

double fleasEval_eval (FleasEval *this) {
  return this->eval;
}

Js *fleasEval_to_js (FleasEval *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, flea_to_js(this->flea));
  arr_push(js, js_wi((int)this->buys));
  arr_push(js, js_wi((int)this->sells));
  arr_push(js, js_wd(this->assets));
  arr_push(js, js_wd(this->profits));
  arr_push(js, js_wd(this->eval));
  return js_wa(js);
}

FleasEval *fleasEval_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  FleasEval *this = MALLOC(FleasEval);
  this->flea = flea_from_js(*p++);
  this->buys = js_ri(*p++);
  this->sells = js_ri(*p++);
  this->assets = js_rd(*p++);
  this->profits = js_rd(*p++);
  this->eval = js_rd(*p++);
  return this;
}

/*--*/

static FleasEval *fleasEval_new (Flea *flea) {
  return _fleasEval_new(flea, 0, 0, 0, 0, -1);
}

// Remove duplicate fleas. If two fleas are equals, the newest one is removed.
// 'fleas' is Arr[FleasEval]
void fleasEval_remove_duplicates (Arr *efleas) {
  for (int i = 1; i < arr_size(efleas); ++i) {
    for (int j = 0; j < i; ++j) {
      Flea *fli = ((FleasEval *)arr_get(efleas, i))->flea;
      Flea *flj = ((FleasEval *)arr_get(efleas, j))->flea;
      if (flea_eq(fli, flj)) {
        if (strcmp(
          flea_name(fli), flea_name(flj)
        ) < 0) {
          arr_set(efleas, j, arr_get(efleas, i));
        }
        arr_remove(efleas, i);
        --i;
        break;
      }
    }
  }
}

// Arr[FleasEval]
static FleasEval *fleasEval_best (Arr *evals) {
  FleasEval *r = (FleasEval *)*arr_start(evals);
  double max = r->eval;
  EACH(evals, FleasEval, ev) {
    if (ev->eval > max) {
      r = ev;
      max = ev->eval;
    }
  }_EACH
  return r;
}

struct FleasCycleRs {
  Arr *efleas; // Arr[FleasResult]
  FleasEval *ebest;
};
typedef struct FleasCycleRs CycleRs;

CycleRs *cycleRs_new (
  Arr *efleas, // Arr[FleasResult]
  FleasEval *ebest
) {
  CycleRs *this = MALLOC(CycleRs);
  this->efleas = efleas;
  this->ebest = ebest;
  return this;
}

// evals is Arr[FleasEval]
static void report(Arr *evals, char *log_id, int ncycle) {
  double sum_assets = 0;
  double sum_profits = 0;
  FleasEval *first = (FleasEval *)*arr_start(evals);
  double min_assets = first->assets;
  double max_assets = first->assets;
  double min_profits = first->profits;
  double max_profits = first->profits;

  EACH(evals, FleasEval, ev) {
    double assets = ev->assets;
    double profits = ev->profits;
    sum_assets += assets;
    if (assets < min_assets) min_assets = assets;
    if (assets > max_assets) max_assets = assets;
    sum_profits += profits;
    if (profits < min_profits) min_profits = profits;
    if (profits > max_profits) max_profits = profits;
  }_EACH

  int n = arr_size(evals);
  flog_info(log_id, str_f(
    "Cycle: %d. Survivers: %d"
    "\n  Avg = Assets: %.2f. Profits: %.4f"
    "\n  Min = Assets: %.2f. Profits: %.4f"
    "\n  Max = Assets: %.2f. Profits: %.4f",
    ncycle, arr_size(evals), sum_assets / n, sum_profits / n, min_assets,
    min_profits, max_assets, max_profits
  ));
}

// Arr[FleasEval]
static Arr *run_cycle (
  Fmodel *model,
  Qtable *opens,
  Qtable *closes,
  int max_sells,
  int min_sells,
  Arr *efleas // Arr[FleasEval]
) {
  if (arr_size(efleas) < 25) {
    log_error(str_f(
      "scheduler/fleas:run_cycle:\n"
      "Number de efleas (%d) less than 25 in model '%s'",
      arr_size(efleas), fmodel_name(model)
    ));
    return efleas;
  }
  // Arr[FleasEval]
  Arr *new_efleas = arr_new();
  double min_assets = +2000000.0;
  double max_assets = -2000000.0;
  double min_profits = +200.0;
  double max_profits = -200.0;
  time_t today = date_now();
  for (;;) {
    EACH(efleas, FleasEval, eval) {
      if (eval->eval < 0) {
        Flea *fl = eval->flea;
        Darr *params = flea_gen(fl);
        Rs *rs = fmodel_assets(model, opens, closes, params);

        int sells = rs_sells(rs);
        if (sells > max_sells || sells < min_sells) continue;

        eval->buys = rs_buys(rs);
        eval->sells = rs_sells(rs);
        eval->assets = rs_assets(rs);
        eval->profits = fmodel_profits_avg(model, opens, closes, params);
      }
      double assets = eval->assets;
      double profits = eval->profits;
      if (assets < min_assets) min_assets = assets;
      if (assets > max_assets) max_assets = assets;
      if (profits < min_profits) min_profits = profits;
      if (profits > max_profits) max_profits = profits;
      arr_push(new_efleas, eval);
    }_EACH

    if (arr_size(new_efleas) > 20) // Assures a minimum number.
      break;

    max_sells *= 1.1;
    min_sells /= 1.1;
    log_error(str_f(
      "All fleas removed by exceding sell limits in '%s'",
      fmodel_name(model)
    ));
    new_efleas = arr_new();
  }

  double assets_dif = max_assets - min_assets;
  double profits_dif = max_profits - min_profits;
  double sum = 0;
  EACH(new_efleas, FleasEval, ev) {
    double e = flea_evaluate(
      ev->flea,
      today,
      (ev->assets - min_assets) / assets_dif,
      (ev->profits - min_profits) / profits_dif
    );
    sum += e;
    ev->eval = e;
  }_EACH

  double avg = sum / arr_size(new_efleas);
  // Arr[FleasEval]
  Arr *tmp = arr_new();
  sum = 0;
  EACH(new_efleas, FleasEval, ev) {
    if (ev->eval > avg) {
      arr_push(tmp, ev);
      sum += ev->eval;
    }
  }_EACH

  avg = sum / arr_size(tmp);
  // Arr[FleasEval]
  Arr *r = arr_new();
  EACH(tmp, FleasEval, ev) {
    if (ev->eval > avg) arr_push(r, ev);
  }_EACH

  return r;
}

CycleRs *selection (
  AsyncActor *ac,
  Qtable *opens,
  Qtable *closes,
  Fmodel *model,
  Opt *log_id // Opt[char]
) {
  // Arr[FleasEval]
  Arr *efleas = arr_new();

  char *date = date_to_str(date_now());
  // Arr[Flea]
  Arr *bests;
  // Arr[Flea]
  Arr *pool;
  void fn1 () {
    bests = fmodels_read_bests(model);
    pool = fmodels_read_pool(model);
  }
  asyncActor_wait(ac, fn1);

  EACH(bests, FleasEval, efl) {
    int new = 1;
    EACH(pool, FleasEval, efl2) {
      if (flea_eq(efl->flea, efl2->flea)) {
        new = 0;
        break;
      }
    }_EACH
    if (new) arr_push(pool, efl);
  }_EACH

  if (!opens || !closes)
    EXC_IO("Fail reading quotes");

  int max_sells = HISTORIC_QUOTES * MAX_SELLS;
  int min_sells = HISTORIC_QUOTES * MIN_SELLS;
  int n_params = darr_size(fmodel_par_mins(model));
  int n_cycles = INSERTION_CYCLE + CYCLES * n_params;
  RANGE(cycle, 1, n_cycles + 1) {
    if (cycle == 1) {
      RANGE0(i, FLEAS_PER_MODEL) {
        arr_push(efleas, fleasEval_new(fmodel_mk_flea(model, date, cycle, i)));
      }_RANGE
    } else if (cycle == INSERTION_CYCLE) {
      EACH(pool, FleasEval, efl) {
        arr_push(efleas, fleasEval_new(efl->flea));
      }_EACH
    } else {
      int n = arr_size(efleas);
      int i = 0;
      while (n < FLEAS_PER_MODEL) {
        // Arr[FleasEval]
        Arr *fleas_tmp = arr_copy(efleas);
        EACH(fleas_tmp, FleasEval, efl) {
          arr_push(efleas, fleasEval_new(
            fmodel_mutate_flea(model, efl->flea, date, cycle, i++)
          ));
          arr_push(
            efleas, fleasEval_new(fmodel_mk_flea(model, date, cycle, i++))
          );
          arr_push(
            efleas, fleasEval_new(fmodel_mk_flea(model, date, cycle, i++))
          );
          if (++n > FLEAS_PER_MODEL)
            break;
        }_EACH
      }
    }

    efleas = run_cycle(
      model, opens, closes, max_sells, min_sells, efleas
    );
    char *lid = opt_nget(log_id);
    if (lid) report(efleas, lid, cycle);
  }_RANGE

  FleasEval *best = fleasEval_best(efleas);
  int cycle = n_cycles;
  for (;;) {
    ++cycle;
    RANGE0(i, FLEAS_PER_MODEL - arr_size(efleas)) {
      arr_push(efleas, fleasEval_new(fmodel_mk_flea(model, date, cycle, i)));
    }_RANGE

    efleas = run_cycle(
      model, opens, closes, max_sells, min_sells, efleas
    );
    char *lid = opt_nget(log_id);
    if (lid) report(efleas, lid, cycle);
    FleasEval *best2 = fleasEval_best(efleas);
    if (flea_eq(best->flea, best2->flea)) break;
  }

  char *lid = opt_nget(log_id);
  if (lid) flog_stop(lid);

  return cycleRs_new(efleas, best);
}

// Arr[FleasEval]
Arr *sort (
  Qtable *opens,
  Qtable *closes,
  Fmodel *model,
  Arr *efleas, // Arr[FleasEval]
  int filtered
) {
  // Arr[FleasEval]
  Arr *r = fleas_evaluate(opens, closes, model, efleas, filtered);
  int fn (FleasEval *f1, FleasEval *f2) { return f1->eval < f2->eval; }
  arr_sort(r, (FCMP)fn);
  return r;
}

// Adds data to ranking pool
void fleas_update (
  AsyncActor *ac,
  Qtable *opens,
  Qtable *closes,
  Fmodel *model,
  Opt *log_id // Opt[char]
) {
  CycleRs *rs = selection(ac, opens, closes, model, log_id);

  // Arr[FleasEval]
  Arr *pool = rs->efleas;
  arr_cat(pool, fmodels_read_pool(model));
  fleasEval_remove_duplicates(pool);
  pool = sort(opens, closes, model, pool, 1);
  fmodels_write_pool(model, arr_take(pool, POOL_NUMBER));

  // Arr[FleasEval]
  Arr *bests = fmodels_read_bests(model);
  arr_insert(bests, 0, rs->ebest);
  fmodels_write_bests(model, arr_take(bests, POOL_NUMBER));

  // Arr[Arr[FleasEval]]
  Arr *rank = fmodels_read_ranking(model);
  if (arr_size(rank)) {
    // Arr[FleasEval]
    Arr *last = arr_get(rank, arr_size(rank) - 1);
    int fn(FleasEval *f1) {
      int fn2 (FleasEval *f2) { return flea_eq(f1->flea, f2->flea); }
      return opt_is_full(it_find(it_from(pool), (FPRED)fn2));
    }
    arr_filter_in(last, (FPRED)fn);

    int fn2 (FleasEval *f1, FleasEval *f2) { return f1->eval < f2->eval; }
    arr_sort(last, (FCMP) fn2);
    last = arr_take(last, RANKING_NUMBER - RANKING_CHANGES);

    for (
      int ix = 0;
      arr_size(last) < RANKING_NUMBER && ix < arr_size(pool);
      ++ix
    ) {
      int fn (FleasEval *fl) {
        return str_eq(
          flea_name(fl->flea),
          flea_name(((FleasEval *)(arr_get(pool, ix)))->flea)
        );
      }
      if (!arr_any(last, (FPRED)fn))
        arr_push(last, arr_get(pool, ix));
    }

    arr_sort(last, (FCMP)fn2);
    arr_push(rank, last);
    rank = arr_drop(rank, arr_size(rank) - HISTORIC_RANKING_ENTRIES);
  } else {
    arr_push(rank, arr_take(pool, RANKING_NUMBER));
  }
  fmodels_write_ranking(model, rank);

  // Arr[Investor]
  Arr *invs = ranking_read_pool();
  Investor *fn(FleasEval *f) { return investor_new(model, f); }
  arr_cat(invs, arr_map(pool, (FCOPY) fn));
  ranking_write_pool(invs);
}

static void run (AsyncActor *ac) {
  Qtable *opens = NULL;
  Qtable *closes = NULL;
  void fn1 () {
    opens = opt_nget(quotes_opens());
    closes = opt_nget(quotes_closes());
  }
  asyncActor_wait(ac, fn1);

  ranking_write_pool(arr_new()); // Rewrites pool through fleas_update
  EACH(fmodels_list(), Fmodel, model) {
    fleas_update(ac, opens, closes, model, opt_empty());
  }_EACH

  // ranking ---------------------------------------------------------

  // Arr[Investor]
  Arr *pool = ranking_read_pool();
  int fn2(Investor *i1, Investor *i2) {
    return investor_eflea(i1)->eval < investor_eflea(i2)->eval;
  }
  arr_sort(pool, (FCMP)fn2);
  // Arr[Arr[Investor]]
  Arr *rank = ranking_read();
  if (arr_size(rank)) {
    // Arr[Investor]
    Arr *last = arr_get(rank, arr_size(rank) - 1);
    int fn(Investor *i1) {
      int fn2 (Investor *i2) {
        return flea_eq(investor_eflea(i1)->flea, investor_eflea(i2)->flea);
      }
      return opt_is_full(it_find(it_from(pool), (FPRED)fn2));
    }
    arr_filter_in(last, (FPRED)fn);

    int fn2 (Investor *i1, Investor *i2) {
      return investor_eflea(i1)->eval < investor_eflea(i2)->eval;
    }
    arr_sort(last, (FCMP) fn2);
    last = arr_take(last, RANKING_NUMBER - RANKING_CHANGES);

    for (
      int ix = 0;
      arr_size(last) < RANKING_NUMBER && ix < arr_size(pool);
      ++ix
    ) {
      int fn (Investor *inv) {
        return str_eq(
          flea_name(investor_eflea(inv)->flea),
          flea_name(investor_eflea(((Investor *)(arr_get(pool, ix))))->flea)
        );
      }
      if (!arr_any(last, (FPRED)fn))
        arr_push(last, arr_get(pool, ix));
    }

    arr_sort(last, (FCMP)fn2);
    arr_push(rank, last);
    rank = arr_drop(rank, arr_size(rank) - HISTORIC_RANKING_ENTRIES);
  } else {
    arr_push(rank, arr_take(pool, RANKING_NUMBER));
  }
  ranking_write(rank);
}

void fleas_run(AsyncActor *ac) {
  async_thread_detached((void (*)(void *))run, ac);
}

// Arr[FleasEval]
Arr *fleas_evaluate (
  Qtable *opens,
  Qtable *closes,
  Fmodel *model,
  Arr *efleas, // Arr[FleasEval]
  int filtered
) {
  int max_sells = HISTORIC_QUOTES * MAX_SELLS;
  int min_sells = HISTORIC_QUOTES * MIN_SELLS;

  // Arr[FleasEval]
  Arr *new_efleas = arr_new();
  double min_assets = +2000000.0;
  double max_assets = -2000000.0;
  double min_profits = +200.0;
  double max_profits = -200.0;
  time_t today = date_now();

  EACH(efleas, FleasEval, eval) {
    Flea *fl = eval->flea;
    Darr *params = flea_gen(fl);
    Rs *rs = fmodel_assets(model, opens, closes, params);

    if (filtered) {
      int sells = rs_sells(rs);
      if (sells > max_sells || sells < min_sells) continue;
    }

    double assets = rs_assets(rs);
    eval->buys = rs_buys(rs);
    eval->sells = rs_sells(rs);
    eval->assets = assets;

    double profits = fmodel_profits_avg(model, opens, closes, params);
    eval->profits = profits;

    if (assets < min_assets) min_assets = assets;
    if (assets > max_assets) max_assets = assets;
    if (profits < min_profits) min_profits = profits;
    if (profits > max_profits) max_profits = profits;
    arr_push(new_efleas, eval);
  }_EACH

  double assets_dif = max_assets - min_assets;
  double profits_dif = max_profits - min_profits;
  EACH(new_efleas, FleasEval, ev) {
    ev->eval = flea_evaluate(
      ev->flea,
      today,
      (ev->assets - min_assets) / assets_dif,
      (ev->profits - min_profits) / profits_dif
    );
  }_EACH

  return new_efleas;
}


