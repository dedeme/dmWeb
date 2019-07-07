// Copyright 23-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "io/accdb.h"
#include "dmc/date.h"
#include "io/io.h"
#include "io/conf.h"
#include "io/quotes.h"
#include "data/dfleas/dfleas__models.h"
#include "data/Model.h"
#include "DEFS.h"

static char *acc_dir = NULL;
static char *diaries_db = NULL;
static char *profits_db = NULL;
static char *quotes_db = NULL;

void accdb_init (void) {
  acc_dir = path_cat(io_data_dir(), "acc", NULL);
  diaries_db = path_cat(acc_dir, "diaries", NULL);
  profits_db = path_cat(acc_dir, "profits", NULL);
  quotes_db = path_cat(acc_dir, "quotes.db", NULL);
  if (!file_exists(acc_dir)) {
    file_mkdir(acc_dir);
    file_mkdir(diaries_db);
    file_mkdir(profits_db);
    file_write(quotes_db, "{}");
  }
}

QmatrixValues accdb_dailyq_read (Arr *nicks) {
  if (!quotes_db) EXC_ILLEGAL_STATE("'quotes_db' was not intiliazed")

  // Map[Js]
  Map *qs = js_ro((Js *)file_read(quotes_db));
  QmatrixValues r = ATOMIC(arr_size(nicks) * sizeof(double));
  EACH_IX(nicks, char, nk, ix) {
    r[ix] = js_rd(opt_oget(map_get(qs, nk), (Js *)"-1"));
  }_EACH
  return r;
}

double accdb_dailyq_read_nick (char *nick) {
  if (!quotes_db) EXC_ILLEGAL_STATE("'quotes_db' was not intiliazed")

  // Map[Js]
  Map *qs = js_ro((Js *)file_read(quotes_db));
  return js_rd(opt_oget(map_get(qs, nick), (Js *)"-1"));
}

void accdb_dailyq_write (Js *quotes) {
  if (!quotes_db) EXC_ILLEGAL_STATE("'quotes_db' was not intiliazed")

  file_write(quotes_db, (char *)quotes);
}

RsHistoric *accdb_historic (char *nick) {
  // Arr[char]
  Arr *dates = arr_new();
  Darr *opens = darr_new();
  Darr *closes = darr_new();
  // Arr[Quote]
  Arr *quotes = quotes_read(nick);
  arr_reverse(quotes);

  char *activity = conf_activity();
  if (!str_eq(activity, ACT_SLEEPING2)) {
    time_t now = date_now();
    if (atoi(date_f(now, "%H")) < ACT_HISTORIC_END) {
      now = date_add(now, -1);
    }
    double q = accdb_dailyq_read_nick(nick);

    arr_remove(quotes, 0);
    arr_push(quotes, quote_new(date_to_str(now), q, q, q, q, 0, 0));
  }

  EACH(quotes, Quote, q)
    arr_push(dates, quote_date(q));
    darr_push(opens, quote_open(q));
    darr_push(closes, quote_close(q));
  _EACH

  ModelParams *mps = dfleas__models_acc();
  return model_historic(
    modelParams_model(mps),
    modelParams_params(mps),
    dates, opens, closes
  );
}

RsHistoric *accdb_historic_without_dailyq (char *nick) {
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

  ModelParams *mps = dfleas__models_acc();
  return model_historic(
    modelParams_model(mps),
    modelParams_params(mps),
    dates, opens, closes
  );
}

Arr *accdb_diary_read (void) {
  if (!diaries_db) EXC_ILLEGAL_STATE("'diaries' was not intiliazed")

  // Arr[char]
  Arr *years = file_dir(diaries_db);
  int fsort (char *y1, char *y2) { return strcmp(y1, y2) > 0; }
  arr_sort(years, (FCMP)fsort);

  // Arr[AccEntry]
  Arr *r = arr_new();
  EACH(years, char, y)
    char *f = path_cat(diaries_db, y, NULL);
    // Arr[Js]
    Arr *data = js_ra((Js *)file_read(f));
    // Arr[AccEntry]
    Arr *es = arr_from_js(arr_get(data, 1), (FFROM)accEntry_from_js);
    int fsort (AccEntry *e1, AccEntry *e2) {
      int cmp = strcmp(accEntry_date(e1), accEntry_date(e2));
      return  cmp > 0 || (cmp == 0 && accEntry_id(e1) > accEntry_id(e2));
    }
    arr_sort(es, (FCMP)fsort);
    EACH(es, AccEntry, e)
      arr_push(r, e);
    _EACH
  _EACH

  return r;
}

Js *accdb_diary_read_js (void) {
  char *start_day = date_to_str(date_add(date_now(), -365));
  int fdrop (AccEntry *e) { return strcmp(accEntry_date(e), start_day) < 0; }
  // Arr[AccEntry]
  Arr *es = accdb_diary_read();
  es = arr_from_it(it_dropf(arr_to_it(es), (FPRED)fdrop));
  arr_reverse(es);
  return arr_to_js(es, (FTO)accEntry_to_js);
}

void accdb_diary_add (AccEntry *entry) {
  if (!diaries_db) EXC_ILLEGAL_STATE("'diaries' was not intiliazed")

  char *year = str_left(accEntry_date(entry), 4);
  char *f = path_cat(diaries_db, year, NULL);
  // Arr[Js]
  Arr *es = arr_new();
  int next_id = 0;
  if (file_exists(f)) {
    // Arr[Js]
    Arr *data = js_ra((Js *)file_read(f));
    next_id = js_ri(arr_get(data, 0));
    es = js_ra(arr_get(data, 1));
  }

  accEntry_set_id(entry, next_id);
  arr_push(es, accEntry_to_js(entry));

  // Arr[Js]
  Arr *data = arr_new();
  arr_push(data, js_wi(next_id + 1));
  arr_push(data, js_wa(es));
  file_write(f, (char *)js_wa(data));
}

void accdb_diary_remove (char *year, int id) {
  if (!diaries_db) EXC_ILLEGAL_STATE("'diaries' was not intiliazed")

  char *f = path_cat(diaries_db, year, NULL);
  if (file_exists(f)) {
    // Arr[Js]
    Arr *data = js_ra((Js *)file_read(f));
    // Arr[Js]
    Arr *es = js_ra(arr_get(data, 1));

    int findex (Js *js) { return accEntry_id(accEntry_from_js(js)) == id; }
    int ix = it_index(arr_to_it(es), (FPRED)findex);

    if (ix != -1) arr_remove(es, ix);

    arr_set(data, 1, js_wa(es));
    file_write(f, (char *)js_wa(data));
  }
}

void accdb_pf_update (AccPf *pf) {
  EACH(pf, AccPfEntry, e)
    char *nick = accPfEntry_nick(e);
    double quote = accdb_dailyq_read_nick(nick);
    accPfEntry_set_quote(e, quote);
    RsHistoric *rs = accdb_historic(nick);
    double ref = rsHistoric_ref(rs);
    accPfEntry_set_ref(e, ref < quote ? ref : quote);
  _EACH
}

Js *accdb_profits_read_js (void) {
  if (!profits_db) EXC_ILLEGAL_STATE("'profits' was not intiliazed")

  // Arr[char]
  Arr *years = file_dir(profits_db);
  arr_sort(years, (FCMP)str_greater);

  if (!arr_size(years)) {
    return (Js *)"[]";
  }

  // Arr[Js]
  Arr *r = arr_new();
  EACH(years, char, year)
    // Arr[Js]
    Arr *jss = js_ra((Js *)file_read(path_cat(profits_db, year, NULL)));
    EACH(jss, Js, js)
      arr_push(r, js);
    _EACH
  _EACH

  return js_wa(r);
}

void accdb_profits_add (char *date, double total, double acc, double risk) {
  if (!profits_db) EXC_ILLEGAL_STATE("'profits' was not intiliazed")

  char *f = path_cat(profits_db, str_left(date, 4), NULL);

  // Arr[Js]
  Arr *jss = arr_new();
  if (file_exists(f)) {
    jss = js_ra((Js *)file_read(f));
  }

  Darr *pfs = darr_new();
  darr_push(pfs, total);
  darr_push(pfs, acc);
  darr_push(pfs, risk);
  // Arr [Js]
  Arr *a = arr_new();
  arr_push(a, js_ws(date));
  arr_push(a, darr_to_js(pfs));

  arr_push(jss, js_wa(a));
  file_write(f, (char *) js_wa(jss));
}

Js *accdb_profits_with (char *date, double total, double acc, double risk) {
  // Arr[Js]
  Arr *jss = js_ra(accdb_profits_read_js());

  Darr *pfs = darr_new();
  darr_push(pfs, total);
  darr_push(pfs, acc);
  darr_push(pfs, risk);
  // Arr [Js]
  Arr *a = arr_new();
  arr_push(a, js_ws(date));
  arr_push(a, darr_to_js(pfs));

  arr_push(jss, js_wa(a));

  return js_wa(jss);
}
