// Copyright 28-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "io/dailydb.h"
#include "dmc/date.h"
#include "io.h"
#include "data/Nick.h"
#include "data/Acc.h"
#include "data/Rs.h"
#include "io/accdb.h"
#include "io/quotes.h"
#include "io/nicks.h"
#include "io/log.h"

static char *dailydb = NULL;

void dailydb_init (void) {
  dailydb = path_cat(io_data_dir(), "daily", NULL);
  if (!file_exists(dailydb)) {
    file_mkdir(dailydb);
  }
}

// Arr[Js].
// 'qs' is Map[Js->double] (nick_name -> close)
// 'pf' is Arr[AccPfEntry]
static Arr *base (Map *qs, Arr *pf, Nick *nk) {
  // Arr[Js]
  Arr *r = arr_new();

  char *nk_name = nick_name(nk);
  Js *quote = opt_oget(map_get(qs, nk_name), NULL);
  if (!quote || js_rd(quote) <= 0) {
    return r;
  }
  Js *hour = js_ws(date_f(date_now(), "%H"));
  // Arr[Js]
  Arr *hq = arr_new();
  arr_push(hq, hour);
  arr_push(hq, quote);
  // Arr[Js]
  Arr *hqs = arr_new();
  arr_push(hqs, js_wa(hq));

  int ffind (AccPfEntry *e) { return str_eq(accPfEntry_nick(e), nk_name); }
  AccPfEntry *pfe = opt_oget(it_find(arr_to_it(pf), (FPRED)ffind), NULL);
  int stocks = 0;
  double price = 0;
  if (pfe) {
    stocks = accPfEntry_stocks(pfe);
    price = accPfEntry_price(pfe);
  }
  RsHistoric *rs = accdb_historic_without_dailyq(nk_name);
  double ref = rsHistoric_ref(rs);

  arr_push(r, js_ws(nk_name));
  arr_push(r, js_wa(hqs));
  arr_push(r, js_wi(stocks));
  arr_push(r, js_wd(price));
  arr_push(r, js_wd(rsHistoric_stocks(rs) ? -ref : ref));
  return r;
}

void dailydb_reset (void) {
  if (!dailydb) EXC_ILLEGAL_STATE("'dailydb' was not intiliazed")

  file_del(dailydb);
  file_mkdir(dailydb);

  // Map[Js->double] (nick_name -> close)
  Map *qs = quotes_last_quotes();
  AccLedPf *rs = accLedPf_new(accdb_diary_read());
  EACH(accLedPf_errors(rs), char, e)
    log_error(e);
  _EACH
  // Arr[AccPfEntry]
  Arr *pf = accLedPf_pf(rs);
  // It is not necessary to use 'accdb_pf_update(pf);'

  EACH(nicks_list(), Nick, nk)
    if (!nick_is_sel(nk)) {
      continue;
    }
    // Arr[Js]
    Arr *b = base(qs, pf, nk);

    if (arr_size(b)) {
      char *f = path_cat(dailydb, str_f("%s", nick_name(nk)), NULL);
      file_write(f, (char *)js_wa(b));
    }
  _EACH
}

void dailydb_update (void) {
  if (!dailydb) EXC_ILLEGAL_STATE("'dailydb' was not intiliazed")

  // Arr[Nick]
  Arr *nicks_missing = arr_new();

  EACH(nicks_list(), Nick, nk)
    if (!nick_is_sel(nk)) {
      continue;
    }
    char *nk_name = nick_name(nk);

    char *f = path_cat(dailydb, str_f("%s", nk_name), NULL);
    if (!file_exists(f)) {
      arr_push(nicks_missing, nk);
      continue;
    }
    // Arr[Js]
    Arr *data = js_ra((Js *)file_read(f));
    // Arr[Js]
    Arr *hqs = arr_get(data, 1);
    // Arr[Js]
    Arr *hq = arr_peek(hqs);

    double q = accdb_dailyq_read_nick(nk_name);
    if (q > 0) {
      Js *qjs = js_wd(q);
      if (!str_eq((char *)arr_get(hq, 1), (char *)qjs)) {
        hq = arr_new();
        arr_push(hq, js_ws(date_f(date_now(), "%H")));
        arr_push(hq, qjs);

        arr_push(hqs, hq);

        file_write(f, (char *)js_wa(data));
      }
    }
  _EACH

  if (arr_size(nicks_missing)) {
    // Map[Js->double] (nick_name -> close)
    Map *qs = quotes_last_quotes();
    AccLedPf *rs = accLedPf_new(accdb_diary_read());
    EACH(accLedPf_errors(rs), char, e)
      log_error(e);
    _EACH
    // Arr[AccPfEntry]
    Arr *pf = accLedPf_pf(rs);
    // It is not necessary to use 'accdb_pf_update(pf);'

    EACH(nicks_missing, Nick, nk)
      if (!nick_is_sel(nk)) {
        continue;
      }
      // Arr[Js]
      Arr *b = base(qs, pf, nk);

      if (arr_size(b)) {
        char *f = path_cat(dailydb, str_f("%s", nick_name(nk)), NULL);
        file_write(f, (char *)js_wa(b));
      }
    _EACH
    dailydb_update();
  }
}

Js *dailydb_cos (void) {
  if (!dailydb) EXC_ILLEGAL_STATE("'dailydb' was not intiliazed")

  // Arr[Js]
  Arr *r = arr_new();
  EACH(file_dir(dailydb), char, f)
    arr_push(r, (Js *)file_read(path_cat(dailydb, f, NULL)));
  _EACH

  return js_wa(r);
}
