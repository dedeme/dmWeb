// Copyright 27-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/acc/acc__trading.h"
#include "dmc/cgi.h"
#include "io/nicks.h"
#include "io/accdb.h"
#include "io/log.h"
#include "data/Acc.h"
#include "data/ModelParams.h"
#include "scheduler/fleas/fleas__models.h"
#include "DEFS.h"

// mrq is Map[Js]
char *acc__trading_process(AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(rq, mrq, "rq")
  // Map[Js]
  Map *rp = map_new();

  if (str_eq(rq, "idata")) {
    void fn (void *null) {
      // Arr[AccEntry]
      Arr *annotations = accdb_diary_read();
      AccLedPf *rs = accLedPf_new(annotations);
      EACH(accLedPf_errors(rs), char, e)
        log_error(e);
      _EACH
      // Arr[AccPfEntry]
      Arr *pf = accLedPf_pf(rs);
      double cash = accLedger_cash(accLedPf_ledger(rs));

      int ffilter (Nick *nk) { return nick_is_sel(nk); }
      void *fmap (Nick *nk) { return nick_name(nk); }
      // Arr[char]
      Arr *nicks = arr_from_it(it_map(
        it_filter(arr_to_it(nicks_list()), (FPRED)ffilter),
        (FCOPY)fmap
      ));
      int nicks_size = arr_size(nicks);
      OrderCos *os = orderCos_new(nicks_size);
      EACH_IX(nicks, char, nick, ix)
        RsHistoric *rs = accdb_historic(nick);
        orderCos_add(os, ix, rsHistoric_order(rs));
      _EACH

      // Arr[Js -> [char, int]] (pairs nick, stocks)
      Arr *sells = arr_new();
      int *scos = orderCos_sells(os);
      REPEAT(orderCos_nsells(os))
        char *nick = arr_get(nicks, *scos++);
        int ffind (AccPfEntry *e) { return str_eq(accPfEntry_nick(e), nick); }
        AccPfEntry *e = opt_oget(it_find(arr_to_it(pf), (FPRED)ffind), NULL);
        if (e) {
          double q = accdb_dailyq_read_nick(nick);
          int stocks = accPfEntry_stocks(e);
          cash += stocks * q;

          Arr *jss = arr_new();
          arr_push(jss, js_ws(nick));
          arr_push(jss, js_wi(stocks));
          arr_push(sells, js_wa(jss));
        }
      _REPEAT

      // Arr[Js -> [char, int]] (pairs nick, stocks)
      Arr *buys = arr_new();
      int *bcos = orderCos_buys(os);
      REPEAT(orderCos_nbuys(os))
        Arr *jss = arr_new();
        char *nick = arr_get(nicks, *bcos++);
        arr_push(jss, js_ws(nick));
        int stocks = 0;
        if (cash > MIN_TO_BET) {
          double q = accdb_dailyq_read_nick(nick);
          stocks = (int)(BET / q);
          cash -= stocks * q;
        }
        arr_push(jss, js_wi(stocks));
        arr_push(buys, js_wa(jss));
      _REPEAT

      map_put(rp, "buys", js_wa(buys));
      map_put(rp, "sells", js_wa(sells));

      ModelParams *mps = fleas__models_acc();
      map_put(rp, "params", darr_to_js(modelParams_params(mps)));
    }
    asyncActor_wait(ac, fn, NULL);
    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT("rq", "idata", rq)
  return NULL; // Unreachable
}


