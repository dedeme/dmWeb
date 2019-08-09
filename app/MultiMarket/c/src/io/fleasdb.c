// Copyright 12-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "io/fleasdb.h"
#include "dmc/date.h"
#include "io/io.h"
#include "io/quotes.h"
#include "io/conf.h"
#include "io/accdb.h"
#include "data/Rs.h"
#include "data/Model.h"
#include "data/Nick.h"
#include "data/dfleas/dfleas__models.h"
#include "DEFS.h"

static char *fleas_dir = NULL;
static char *bests_dir = NULL;
static char *charts_dir = NULL;
static char *champions_dir = NULL;
static char *ranking_db = NULL;
static char *flog_db = NULL;

void fleasdb_init() {
  fleas_dir = path_cat(io_data_dir(), "fleas", NULL);
  bests_dir = path_cat(fleas_dir, "_bests", NULL);
  charts_dir = path_cat(fleas_dir, "_charts", NULL);
  champions_dir = path_cat(fleas_dir, "_champions", NULL);
  ranking_db = path_cat(fleas_dir, "ranking.db", NULL);
  flog_db = path_cat(fleas_dir, "flog.db", NULL);
  if (!file_exists(fleas_dir)) {
    file_mkdir(fleas_dir);
    file_mkdir(bests_dir);
    file_mkdir(charts_dir);
    file_mkdir(champions_dir);
    file_write(ranking_db, "[]");
    file_write(flog_db, "[]");
  }
}

// Returns Arr[char]
Arr *fleasdb_model_dates (char * model) {
  if (!fleas_dir) EXC_ILLEGAL_STATE("'fleas_dir' was not intiliazed");

  return file_dir(path_cat(fleas_dir, model, NULL));
}

// Returns Js -> Arr[RsWeb]
Js *fleasdb_model_read_js (char *model, char *date) {
  if (!fleas_dir) EXC_ILLEGAL_STATE("'fleas_dir' was not intiliazed")

  char *f = path_cat(fleas_dir, model, date, NULL);
  if (!file_exists(f)){
    return (Js *)"[]";
  }

  return (Js *)file_read(f);
}

// 'rs' is Arr[RsWeb]
void fleasdb_model_write (char *model, Js *params, char *date, Arr *rs) {
  if (!fleas_dir) EXC_ILLEGAL_STATE("'fleas_dir' was not intiliazed")

  char *dir = path_cat(fleas_dir, model, NULL);
  if (!file_exists(dir)) {
    file_mkdir(dir);
  }

  char *f = path_cat(dir, date, NULL);
  file_write(f, (char *)arr_to_js(rs, (FTO)rsWeb_to_js));

  // Arr[char]
  Arr *fs = file_dir(dir);
  int fsort (char *f1, char *f2) { return strcmp(f1, f2) < 0; }
  arr_sort(fs, (FCMP)fsort);
  RANGE(i, FLEA_MODEL_DATES, arr_size(fs))
    file_del(arr_get(fs, i));
  _RANGE
}

// Returns Arr[RsBests]
Arr *fleasdb_bests_read (char *model) {
  if (!bests_dir) EXC_ILLEGAL_STATE("'bests_dir' was not intiliazed")

  char *f = path_cat(bests_dir, str_f("%s.db", model), NULL);
  if (!file_exists(f)) {
    return arr_new();
  }

  return arr_from_js((Js *)file_read(f), (FFROM)rsBests_from_js);
}

// Returns Js -> Arr[RsBests]
Js *fleasdb_bests_read_js (char *model) {
  if (!bests_dir) EXC_ILLEGAL_STATE("'bests_dir' was not intiliazed")

  char *f = path_cat(bests_dir, str_f("%s.db", model), NULL);
  if (!file_exists(f)) {
    return (Js *)"[]";
  }

  return (Js *)file_read(f);
}

void fleasdb_bests_add (char *model, RsBests *rs) {
  // Arr[RsBests]
  Arr *a = fleasdb_bests_read(model);

  char *date = rsBests_date(rs);
  int included = 0;
  EACH_IX(a, RsBests, r, ix)
    int cmp = strcmp(date, rsBests_date(r));
    if (!cmp) {
      arr_set(a, ix, rs);
      included = 1;
      break;
    } else if (cmp > 0) {
      arr_insert(a, ix, rs);
      included = 1;
      break;
    }
  _EACH

  if (!included) {
    arr_push(a, rs);
  }

  while (arr_size(a) > MAXIMUM_HISTORIC_BESTS) {
    arr_remove(a, MAXIMUM_HISTORIC_BESTS);
  }

  char *f = path_cat(bests_dir, str_f("%s.db", model), NULL);
  file_write(f, (char *)arr_to_js(a, (FTO)rsBests_to_js));
}

// Returns Arr[RsChart]
Arr *charts_read (int cache, char *model) {
  if (!charts_dir) EXC_ILLEGAL_STATE("'charts_dir' was not intiliazed")

  char *f = path_cat(charts_dir, str_f("%s.db", model), NULL);
  if (!file_exists(f)) {
    return arr_new();
  } else {
    return arr_from_js((Js *)file_read(f), (FFROM)rsChart_from_js);
  }
}

Js *fleasdb_charts_read_nicks_js (char *model) {
  // Arr[Char]
  Arr *a = arr_new();
  EACH(charts_read(0, model), RsChart, rs)
    arr_push(a, rsChart_nick(rs));
  _EACH
  return arr_to_js(a, (FTO)js_ws);
}

/// Returns Js -> Opt[RsChart]
Js *fleasdb_charts_read_js (char *model, char *nick) {
  int fn (RsChart *rs) { return str_eq(rsChart_nick(rs), nick); }
  //Opt[RsChart]
  Opt *rs = it_find(arr_to_it(charts_read(1, model)), (FPRED)fn);
  if (opt_is_empty(rs)) {
    return js_wn();
  }
  return rsChart_to_js(opt_get(rs));
}

void fleasdb_charts_write (char *model, RsCharts *rs) {
  if (!charts_dir) EXC_ILLEGAL_STATE("'charts_dir' was not intiliazed")

  char *f = path_cat(charts_dir, str_f("%s.db", model), NULL);
  file_write(f, (char *)arr_to_js(rsCharts_cos(rs), (FTO)rsChart_to_js));
}

void fleasdb_flog_write (char *msg) {
  Js *ajs = fleasdb_flog_to_js();
  // Arr[char]
  Arr *a = arr_from_js(ajs, (FFROM)js_rs);
  arr_insert(a, 0, msg);
  file_write(flog_db, (char *)arr_to_js(a, (FTO)js_ws));
}

// Returns Arr[RsChampions]
Arr *fleasdb_champions_read (int nparams) {
  if (!champions_dir) EXC_ILLEGAL_STATE("'champions_dir' was not intiliazed")

  char *f = path_cat(champions_dir, str_f("%d.db", nparams), NULL);
  if (!file_exists(f)) {
    return arr_new();
  }

  return arr_from_js((Js *)file_read(f), (FFROM)rsChampions_from_js);
}

/// Returns Js -> Arr[RsChampions]
Js *fleasdb_champions_read_js (int nparams) {
  if (!champions_dir) EXC_ILLEGAL_STATE("'champions_dir' was not intiliazed")

  // Arr[char]
  Arr *models = dfleas__models_names();

  int ffilter (RsChampions *rs) {
    char *model = rsChampions_model(rs);
    int fcontains (char *md) { return str_eq(model, md); }
    return it_contains(it_from(models), (FPRED)fcontains);
  }
  return arr_to_js(
    it_to(it_filter(it_from(fleasdb_champions_read(nparams)), (FPRED)ffilter)),
    (FTO)rsChampions_to_js
  );
}

// Returns Js -> Opt[RsChart]
Js *fleasdb_champions_chart_read_js (
  int nparams, char *model, char *nick, char *flea
) {
  int ffind (RsChampions *rs) {
    if (str_eq(rsChampions_model(rs), model)) {
      return str_eq(
        flea_name(rs_flea(rsWeb_result(rsChampions_result(rs)))),
        flea
      );
    }
    return 0;
  }
  RsChampions *rsCh = opt_oget(
    it_find(arr_to_it(fleasdb_champions_read(nparams)), (FPRED)ffind),
    NULL
  );
  if (!rsCh) return js_wn();

  Model *md = opt_oget(dfleas__models_get(model), NULL);
  if (!md) return js_wn();

  Darr *params = rsWeb_params(rsChampions_result(rsCh));

  // Arr[char]
  Arr *dates = arr_new();
  Darr *opens = darr_new();
  Darr *closes = darr_new();
  // Arr[Quote]
  Arr *quotes = quotes_read(nick);
  arr_reverse(quotes);

  EACH(quotes, Quote, q)
    arr_push(dates, quote_date(q));
    darr_push(opens, quote_open(q));
    darr_push(closes, quote_close(q));
  _EACH

  RsHistoric *rsH = model_historic(md, params, dates, opens, closes);

  return rsChart_to_js(rsChart_new(
    nick,
    rsHistoric_profits(rsH),
    rsHistoric_quotes(rsH),
    rsHistoric_historic(rsH)
  ));
}

void fleasdb_champions_add (RsChampions *rs) {
  char *model = rsChampions_model(rs);
  RsWeb *rsW = rsChampions_result(rs);
  char *fname = flea_name(rs_flea(rsWeb_result(rsW)));
  int nparams = darr_size(rsWeb_params(rsW));
  // Arr[RsChampions]
  Arr *rss = fleasdb_champions_read(nparams);
  int findex (RsChampions *r) {
    char *m = rsChampions_model(r);
    char *fnm = flea_name(rs_flea(rsWeb_result(rsChampions_result(r))));
    return str_eq(m, model) && str_eq(fnm, fname);
  }
  int ix = it_index(arr_to_it(rss), (FPRED)findex);
  if (ix == -1) {
    arr_push(rss, rs);
    fleasdb_champions_write(nparams, rss);
  }
}

/// 'rs' is Arr[RsChampions]
void fleasdb_champions_write (int nparams, Arr *rss) {
  if (!champions_dir) EXC_ILLEGAL_STATE("'champions_dir' was not intiliazed")

  char *f = path_cat(champions_dir, str_f("%d.db", nparams), NULL);
  file_write(f, (char *)arr_to_js(rss, (FTO)rsChampions_to_js));
}

// Returns Arr[RsChampions]
Arr *fleasdb_ranking (void) {
  int fsort (RsChampions *r1, RsChampions *r2) {
    return rsAssets_assets(rs_assets(rsWeb_result(rsChampions_result(r1)))) <
      rsAssets_assets(rs_assets(rsWeb_result(rsChampions_result(r2))));
  }
  It *remove_duplicates (It *it) {
    int feq (RsChampions *r1, RsChampions *r2) {
      return str_eq(
        flea_name(rs_flea(rsWeb_result(rsChampions_result(r1)))),
        flea_name(rs_flea(rsWeb_result(rsChampions_result(r2))))
      );
    }
    Arr *dup;
    Arr *rest;
    it_duplicates(&dup, &rest, it, (FCMP)feq);
    return it_from(rest);
  }

  Arr *r = arr_from_js((Js *)file_read(ranking_db), (FFROM)rsChampions_from_js);
  if (arr_size(r)) {
    r = it_to(
      it_take(
        it_sort(
          remove_duplicates(it_cat(
            it_from(r),
            it_cat(
              it_take(it_from(fleasdb_champions_read(1)), 1),
              it_cat(
                it_take(it_from(fleasdb_champions_read(2)), 1),
                it_cat(
                  it_take(it_from(fleasdb_champions_read(3)), 1),
                  it_take(it_from(fleasdb_champions_read(4)), 1)
                )
              )
            )
          )),
          (FCMP)fsort
        ),
        40
      )
    );
  } else {
    r = it_to(
      it_sort(
        it_cat(
          it_take(it_from(fleasdb_champions_read(1)), 10),
          it_cat(
            it_take(it_from(fleasdb_champions_read(2)), 10),
            it_cat(
              it_take(it_from(fleasdb_champions_read(3)), 10),
              it_take(it_from(fleasdb_champions_read(4)), 10)
            )
          )
        ),
        (FCMP)fsort
      )
    );
  }

  file_write(ranking_db, (char *)arr_to_js(r, (FTO)rsChampions_to_js));
  return r;
}

// Returns Arr[Arr[Opt[RankAssets]]]
// ranking is Arr[RsChampions]
Arr *fleasdb_ranking_assets (Arr *ranking) {
  if (!arr_size(ranking)){
    return arr_new();
  }

  // Arr[char]
  Arr *dates = quotes_dates();
  Qmatrix *opens = opt_nget(quotes_opens());
  Qmatrix *closes = opt_nget(quotes_closes());
  if (arr_size(dates) != HISTORIC_QUOTES || !opens || !closes) {
    EXC_ILLEGAL_STATE(
      "arr_size(dates) != HISTORIC_QUOTES || !opens || !closes"
    )
  }

  if (!str_eq(conf_activity(), ACT_SLEEPING2)) {
    time_t now = date_now();
    if (str_greater("06", date_f(now, "%H"))) {
      now = date_add(now, -1);
    }
    arr_remove(dates, 0);
    arr_push(dates, date_to_str(now));
    char *fn (Nick *nk) { return nick_name(nk); }
    // Arr[char]
    Arr *nicks = it_to(it_map(it_from(qmatrix_nicks(opens)), (FCOPY)fn));
    QmatrixValues row = accdb_dailyq_read(nicks);
    qmatrix_add(qmatrix_values(opens), row);
    qmatrix_add(qmatrix_values(closes), row);
  }

  // Arr[Arr[Opt[RankAssets]]]
  Arr *r = arr_new();
  EACH(ranking, RsChampions, rs) {
    Model *model = opt_nget(dfleas__models_get(rsChampions_model(rs)));
    if (!model) {
      arr_push(r, arr_new());
      continue;
    }

    Darr *params = rsWeb_params(rsChampions_result(rs));
    // Arr[RankAssets]
    Arr *asts = model_assets_historic(model, params, dates, opens, closes);
    if (arr_size(asts) > MAXIMUM_HISTORIC_RANKING) {
      arr_remove_range(asts, 0, arr_size(asts) - MAXIMUM_HISTORIC_RANKING);
    }
    arr_push(r, asts);
  }_EACH

  return r;
}

Js *fleasdb_flog_to_js (void) {
  if (!flog_db) EXC_ILLEGAL_STATE("'flog.db' was not intiliazed")

  return (Js *)file_read(flog_db);
}

void fleasdb_flog_clear (void) {
  file_write(flog_db, "[]");
}
