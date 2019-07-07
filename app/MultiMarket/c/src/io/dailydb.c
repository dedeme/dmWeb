// Copyright 28-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "io/dailydb.h"
#include "dmc/date.h"
#include "io/io.h"
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
  arr_push(r, js_wd(
    (rsHistoric_stocks(rs) && !order_is_sell(rsHistoric_order(rs))) ||
    order_is_buy(rsHistoric_order(rs))
    ? -ref : ref
  ));
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

  int size = 0;
  int changed = 0;
  // Arr[Nick]
  Arr *all_nicks = nicks_list();

  // Arr[Nick]
  Arr *nicks_missing = arr_new();

  EACH(all_nicks, Nick, nk)
    if (!nick_is_sel(nk)) {
      continue;
    }
    char *nk_name = nick_name(nk);

    char *f = path_cat(dailydb, str_f("%s", nk_name), NULL);
    if (!file_exists(f)) {
      arr_push(nicks_missing, nk);
      changed = 1;
      continue;
    }

    if (!changed) {
      // Arr[Js]
      Arr *data = js_ra((Js *)file_read(f));
      // Arr[Js]
      Arr *hqs = js_ra(arr_get(data, 1));
      if (arr_size(hqs) > size) size = arr_size(hqs);
      // Arr[Js]
      Arr *hq = js_ra(arr_peek(hqs));

      double q = accdb_dailyq_read_nick(nk_name);
      if (q > 0) {
        Js *qjs = js_wd(q);
        changed = !str_eq((char *)arr_get(hq, 1), (char *)qjs);
      }
    }
  _EACH

  if (!changed) return;

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
      // Arr[Js]
      Arr *b = base(qs, pf, nk);

      if (arr_size(b)) {
        // Arr[Js]
        Arr *hqs = js_ra(arr_get(b, 1));
        while (arr_size(hqs) < size) {
          arr_push(hqs, arr_peek(hqs));
        }

        char *f = path_cat(dailydb, str_f("%s", nick_name(nk)), NULL);
        file_write(f, (char *)js_wa(b));
      }
    _EACH
  }

  EACH(all_nicks, Nick, nk)
    if (!nick_is_sel(nk)) {
      continue;
    }
    char *nk_name = nick_name(nk);

    char *f = path_cat(dailydb, str_f("%s", nk_name), NULL);
    if (!file_exists(f)) {
      continue;
    }

    // Arr[Js]
    Arr *data = js_ra((Js *)file_read(f));
    // Arr[Js]
    Arr *hqs = js_ra(arr_get(data, 1));
    while (arr_size(hqs) < size) {
      arr_push(hqs, arr_peek(hqs));
    }

    Js *qjs = arr_peek(hqs);
    // Arr[Js]
    Arr *hq = js_ra(qjs);

    double q = accdb_dailyq_read_nick(nk_name);
    if (q > 0) qjs = js_wd(q);

    hq = arr_new();
    arr_push(hq, js_ws(date_f(date_now(), "%H")));
    arr_push(hq, qjs);

    arr_push(hqs, js_wa(hq));
    arr_set(data, 1, js_wa(hqs));

    file_write(f, (char *)js_wa(data));
  _EACH

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

void dailydb_update_charts (void) {
  // Arr[Js]
  Arr *cos = js_ra(dailydb_cos());

  AccLedPf *rs = accLedPf_new(accdb_diary_read());
  EACH(accLedPf_errors(rs), char, e)
    log_error(e);
  _EACH
  // Arr[AccPfEntry]
  Arr *pf = accLedPf_pf(rs);
  // It is not necessary to use 'accdb_pf_update(pf);'

  EACH_IX(cos, Js, co, ix)
    // Arr[Js]
    Arr *aco = js_ra(co);
    char *nick = js_rs(arr_get(aco, 0));
    int stocks = 0;
    double price = 0;
    EACH(pf, AccPfEntry, e)
      if (str_eq(accPfEntry_nick(e), nick)) {
        stocks = accPfEntry_stocks(e);
        price = accPfEntry_price(e);
        break;
      }
    _EACH
    arr_set(aco, 2, js_wi(stocks));
    arr_set(aco, 3, js_wd(price));

    char *f = path_cat(dailydb, str_f("%s", nick), NULL);
    file_write(f, (char *)js_wa(aco));
  _EACH

  dailydb_update();
}
