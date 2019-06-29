// Copyright 29-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "scheduler/fleas.h"
#include "dmc/date.h"
#include "dmc/Iarr.h"
#include "scheduler/fleas/fleas__models.h"
#include "data/Model.h"
#include "data/Rs.h"
#include "io/log.h"
#include "io/fleasdb.h"
#include "io/conf.h"
#include "io/quotes.h"
#include "DEFS.h"

static char *replicate (char ch, int n) {
  Buf *bf = buf_new();
  REPEAT(n)
    buf_cadd(bf, ch);
  _REPEAT
  return buf_to_str(bf);
}

static char *title (char *name) {
  int len = strlen(name);
  return str_f("%s\n%s\n%s", replicate('_', len), name, replicate('T', len));
}

static double davg (double old, double new) {
  return (old * CHAMPIONS_AVG - old + new) / CHAMPIONS_AVG;
}

static int iavg (int old, int new) {
  return (int)(
    (((double)old) * CHAMPIONS_AVG - ((double)old) + ((double)new)) /
    CHAMPIONS_AVG
  );
}

static void run (AsyncActor *ac) {
  void fnLog1 (void *null) {
    log_info("Fleas started");
  }
  asyncActor_run(ac, fnLog1, NULL);

  // Arr[char]
  Arr *dates;
  Qmatrix *closes;
  Qmatrix *opens;
  void fn (void *null) {
    conf_set_fleas_running(1);
    fleasdb_flog_clear();
    dates = quotes_dates();
    closes = opt_oget(quotes_closes(), NULL);
    opens = opt_oget(quotes_opens(), NULL);
  }
  asyncActor_wait(ac, fn, NULL);

  if (!arr_size(dates) || !closes || !opens) {
    return;
  }

  int fleas_running = 1;
  char *date = arr_get(dates, HISTORIC_QUOTES - 1);
  int min_sells = HISTORIC_QUOTES / MIN_SELLS;
  int max_sells = HISTORIC_QUOTES / MAX_SELLS;
  double max_sells_sum = (double)max_sells / 2;
  double max_sells_mul =
      (double)max_sells / (INITIAL_CAPITAL + INITIAL_CAPITAL);
  // Arr[Model]
  Arr *models = fleas__models();

  EACH(models, Model, md) {
    asyncActor_wait(
      ac, (FPROC)fleasdb_flog_write, str_f("%s\n", title(model_name(md)))
    );

    int nparams = arr_size(model_param_names(md));

    Rs *rs_selected = rs_new(
      flea_new(date, 0, 0, nparams),
      rsAssets_new(0, 0, 0),
      rsProfits_new(0, 0, 0)
    );
    Rs *old_rs_selected = rs_selected;

    // Arr[Rs]
    Arr *rss_old = arr_new();
    EACH(
      rsBests_distinct(fleasdb_bests_read(model_name(md))),
      RsBests,
      rs
    )
      Flea *f = rs_flea(rsWeb_result(rsBests_result(rs)));
      RsAssets *assets = model_assets(md, f, opens, closes);
      arr_push(rss_old, rs_new(f, assets, rsProfits_new(0, 0, 0)));
    _EACH

    // Arr[Rs]
    Arr *rss = arr_new();
    for (;;) {
      rss = arr_new();
      arr_push(rss, rs_selected);
      RANGE(i, 1, FLEAS_PER_MODEL / 2)
        arr_push(rss, rs_new(
          flea_new(date, 0, i, nparams),
          rsAssets_new(0, 0, 0),
          rsProfits_new(0, 0, 0)
        ));
      _RANGE

      // Calculate Assets ------------------------------------------------------

      int max_cycles = INSERTION_CYCLE + CYCLES * nparams;
      RANGE0(cy, max_cycles)
        asyncActor_wait(
          ac,
          (FPROC)fleasdb_flog_write,
          str_f("Model: %s. Cycle: %d / %d", model_name(md), cy + 1, max_cycles)
        );

        if (cy == INSERTION_CYCLE || cy == max_cycles - 1) {
          arr_insert_arr(rss, 0, rss_old);
        }

        int rss_size = arr_size(rss);
        int rss_sizec = 0;
        int id = 0;
        REPEAT(FLEAS_PER_MODEL - rss_size)
          Flea *fnew = flea_mutate(
            rs_flea(arr_get(rss, rss_sizec)), date, cy + 1, id++
          );
          RsAssets *assets = model_assets(md, fnew, opens, closes);
          arr_push(rss, rs_new(fnew, assets, rsProfits_new(0, 0, 0)));

          ++rss_sizec;
          if (rss_sizec == rss_size) {
            rss_sizec = 0;
          }
        _REPEAT

        // arr_size(rss) == FLEAS_PER_MODEL
        int max_final_size = FLEAS_PER_MODEL * 0.75;
        do {
          int size = arr_size(rss);
          Arr *rss_sel = arr_new();
          double sum = 0;
          double sum_sel = 0;
          EACH(rss, Rs, rs)
            RsAssets *rsa = rs_assets(rs);
            double assets = rsAssets_assets(rsa);
            sum += assets;
            int sells = rsAssets_sells(rsa);
            if (
              sells >= min_sells &&
              sells <= max_sells_sum + assets * max_sells_mul
            ) {
              arr_push(rss_sel, rs);
              sum_sel += assets;
            }
          _EACH

          if (arr_size(rss_sel)) {
            rss = rss_sel;
            sum = sum_sel;
          }
          double avg = sum / arr_size(rss);

          rss_sel = arr_new();
          EACH(rss, Rs, rs)
            if (rsAssets_assets(rs_assets(rs)) > avg) {
              arr_push(rss_sel, rs);
            }
          _EACH
          if (arr_size(rss_sel)) rss = rss_sel;

          if (arr_size(rss) == size) {
            arr_remove_range(rss, max_final_size, size);
          }
        } while (arr_size(rss) > max_final_size);

        double sum = 0;
        int size = arr_size(rss);
        EACH(rss, Rs, rs)
          sum += rsAssets_assets(rs_assets(rs));
        _EACH
        double avg = sum / size;
        asyncActor_wait(
          ac,
          (FPROC)fleasdb_flog_write,
          str_f("Survivers: %d. Avg: %.2f", size, avg)
        );
      _RANGE

      // Calculate profits -----------------------------------------------------

      double max_assets = -INITIAL_CAPITAL;
      EACH_IX(rss, Rs, rs, i)
        Flea *fl = rs_flea(rs);
        RsAssets *assets = rs_assets(rs);
        double as = rsAssets_assets(assets);
        RsProfits *prfs = model_profits(md, fl, opens, closes);
        arr_set(rss, i, rs_new(rs_flea(rs), assets, prfs));
        if (as > max_assets) max_assets = as;
      _EACH

      double max_sel = -1000;
      int rss_size = arr_size(rss);
      EACH_IX(rss, Rs, rs, ix)
        if(
          rsProfits_sel(rs_profits(rs)) > max_sel &&
          rsAssets_assets(rs_assets(rs)) > max_assets * CUT_PROFITS
        ) {
          rs_selected = rs;
          Flea *f = rs_flea(rs);
          RsProfits *ps = rs_profits(rs);
          max_sel = rsProfits_sel(ps);
          asyncActor_wait(
            ac,
            (FPROC)fleasdb_flog_write,
            str_f(
              "Selected Flea (%d/%d): %s-%d-%d -> %.4f * (1 - %.4f) = %.4f",
              ix, rss_size, flea_date(f), flea_cycle(f), flea_id(f),
              rsProfits_avg(ps), rsProfits_var(ps), max_sel
            )
          );
        }
      _EACH

      void fn (void *null) { fleas_running = conf_fleas_running(); }
      asyncActor_wait(ac, fn, NULL);

      if (
        !fleas_running ||
        rsProfits_sel(rs_profits(old_rs_selected)) >=
        rsProfits_sel(rs_profits(rs_selected))
      ) {
        rs_selected = old_rs_selected;

        Flea *f = rs_flea(rs_selected);
        RsProfits *ps = rs_profits(rs_selected);
        max_sel = rsProfits_sel(ps);
        asyncActor_wait(
          ac,
          (FPROC)fleasdb_flog_write,
          str_f(
            "Flea Finally Selected: %s-%d-%d -> %.4f * (1 - %.4f) = %.4f",
            flea_date(f), flea_cycle(f), flea_id(f),
            rsProfits_avg(ps), rsProfits_var(ps), max_sel
          )
        );
        break;
      }
      old_rs_selected = rs_selected;
    }

    if (!fleas_running) {
      break;
    }

    RsCharts *charts = model_charts(
      md, rs_flea(rs_selected), dates, opens, closes
    );

    void fn (void *null) {
      // Arr[RsWeb]
      Arr *rs_web = arr_new();
      EACH(rss, Rs, rs)
        arr_push(rs_web, rsWeb_new(rs, model_params(md, rs_flea(rs))));
      _EACH
      fleasdb_model_write(
        model_name(md), model_param_jss(md), date, rs_web
      );

      fleasdb_bests_add(model_name(md), rsBests_new(
        date, rsWeb_new(rs_selected, model_params(md, rs_flea(rs_selected)))
      ));

      fleasdb_charts_write(model_name(md), charts);

      fleasdb_champions_add(rsChampions_new(
        model_name(md),
        rsWeb_new(rs_selected, model_params(md, rs_flea(rs_selected)))
      ));
    }
    asyncActor_wait(ac, fn, NULL);

  }_EACH

  // Calculate champions -------------------------------------------------------

  Iarr *nparamss = iarr_new();
  EACH(models, Model, md)
    int nparams = arr_size(model_param_names(md));
    int missing = 1;
    IEACH(nparamss, n)
      if (n == nparams) {
        missing = 0;
        break;
      }
    _EACH
    if (missing) iarr_push(nparamss, nparams);
  _EACH

  IEACH(nparamss, nparams)
    void fn (void *null) {
      // Arr[RsChampions]
      Arr *rss = arr_new();
      EACH_IX(fleasdb_champions_read(nparams), RsChampions, rsCh, ix)
        char *model = rsChampions_model(rsCh);
        // Opt[Model]
        Opt *omd = fleas__models_get(model);
        if (opt_is_empty(omd)) {
          log_error(str_f("Champions model '%s' not found", model));
          continue;
        }
        Model *md = opt_get(omd);
        RsWeb *rsW = rsChampions_result(rsCh);
        Darr *params = rsWeb_params(rsW);
        Rs *rs = rsWeb_result(rsW);
        Flea *fl = rs_flea(rs);
        RsAssets *old_assets = rs_assets(rs);
        RsProfits *old_profits = rs_profits(rs);
        RsAssets *assets = model_assets(md, fl, opens, closes);
        RsProfits *profits = model_profits(md, fl, opens, closes);
        double avg = davg(rsProfits_avg(old_profits), rsProfits_avg(profits));
        double var = davg(rsProfits_var(old_profits), rsProfits_var(profits));
        Rs *new_rs = rs_new(
          fl,
          rsAssets_new(
            davg(rsAssets_assets(old_assets), rsAssets_assets(assets)),
            iavg(rsAssets_buys(old_assets), rsAssets_buys(assets)),
            iavg(rsAssets_sells(old_assets), rsAssets_sells(assets))
          ),
          rsProfits_new(avg, var, avg * (1 - var))
        );
        arr_push(rss, rsChampions_new(model, rsWeb_new(new_rs, params)));
      _EACH

      int fsort (RsChampions *r1, RsChampions *r2) {
        double s1 = rsProfits_sel(
          rs_profits(rsWeb_result(rsChampions_result(r1)))
        );
        double s2 = rsProfits_sel(
          rs_profits(rsWeb_result(rsChampions_result(r2)))
        );
        return s2 > s1;
      }
      arr_sort(rss, (FCMP)fsort);
      if (arr_size(rss) > TOTAL_CHAMPIONS) {
        arr_remove_range(rss, TOTAL_CHAMPIONS, arr_size(rss));
      }
      fleasdb_champions_write(nparams, rss);
    }
    asyncActor_run(ac, fn, NULL);
  _EACH

  void fn2 (void *null) {
    conf_set_fleas_running(0);
  }
  asyncActor_wait(ac, fn2, NULL);

  void fnLog2 (void *null) {
    log_info("Fleas finished");
  }
  asyncActor_run(ac, fnLog2, NULL);
}

void fleas_run(AsyncActor *ac) {
  async_thread((FPROC)run, ac);
}
