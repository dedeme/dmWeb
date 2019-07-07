// Copyright 12-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "io/fleasdb.h"
#include "io/io.h"
#include "io/quotes.h"
#include "data/Rs.h"
#include "data/Model.h"
#include "data/dfleas/dfleas__models.h"
#include "DEFS.h"

static char *fleas_dir = NULL;
static char *bests_dir = NULL;
static char *charts_dir = NULL;
static char *champions_dir = NULL;
static char *bests_db = NULL;
static char *flog_db = NULL;

// Arr[RsChart]
static Arr *charts = NULL;

void fleasdb_init() {
  fleas_dir = path_cat(io_data_dir(), "fleas", NULL);
  bests_dir = path_cat(fleas_dir, "_bests", NULL);
  charts_dir = path_cat(fleas_dir, "_charts", NULL);
  champions_dir = path_cat(fleas_dir, "_champions", NULL);
  bests_db = path_cat(fleas_dir, "bests.db", NULL);
  flog_db = path_cat(fleas_dir, "flog.db", NULL);
  if (!file_exists(fleas_dir)) {
    file_mkdir(fleas_dir);
    file_mkdir(bests_dir);
    file_mkdir(charts_dir);
    file_mkdir(champions_dir);
    file_write(bests_db, "[]");
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

  if (cache) {
    return charts ? charts : arr_new();
  }

  char *f = path_cat(charts_dir, str_f("%s.db", model), NULL);
  if (!file_exists(f)) {
    charts = arr_new();
  } else {
    charts = arr_from_js((Js *)file_read(f), (FFROM)rsChart_from_js);
  }

  return charts;
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

  char *f = path_cat(champions_dir, str_f("%d.db", nparams), NULL);
  if (!file_exists(f)) {
    return (Js *)"[]";
  }

  return (Js *)file_read(f);
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

Js *fleasdb_flog_to_js (void) {
  if (!flog_db) EXC_ILLEGAL_STATE("'flog.db' was not intiliazed")

  return (Js *)file_read(flog_db);
}

void fleasdb_flog_clear (void) {
  file_write(flog_db, "[]");
}
