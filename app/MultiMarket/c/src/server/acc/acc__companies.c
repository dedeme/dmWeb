// Copyright 24-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/acc/acc__companies.h"
#include "dmc/cgi.h"
#include "dmc/date.h"
#include "io/conf.h"
#include "io/nicks.h"
#include "io/accdb.h"
#include "io/log.h"
#include "io/servers.h"
#include "io/quotes.h"
#include "io/calendar.h"
#include "io/accdb.h"
#include "data/dfleas/dfleas__models.h"
#include "data/Acc.h"
#include "data/Model.h"
#include "data/Quote.h"
#include "data/ModelParams.h"
#include "DEFS.h"

// mrq is Map[Js]
char *acc__companies_process(AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(rq, mrq, "rq")
  // Map[Js]
  Map *rp = map_new();

  if (str_eq(rq, "nicks")) {
    int filter_sel (Nick *nk) { return nick_is_sel(nk); }
    void *fmap (Nick *nk) { return nick_name(nk); }
    void fn (void *null) {
      int all_cos = conf_acc_all_cos();
      map_put(rp, "allCos", js_wb(all_cos));

      // It[char]
      It *nicks = it_map(
        it_filter(arr_to_it(nicks_list()), (FPRED)filter_sel),
        (FCOPY)fmap
      );
      if (!all_cos) {
        // Arr[AccEntry]
        Arr *annotations = accdb_diary_read();
        AccLedPf *rs = accLedPf_new(annotations);
        EACH(accLedPf_errors(rs), char, e)
          log_error(e);
        _EACH
        // Arr[AccPfEntry]
        Arr *pf = accLedPf_pf(rs);
        int filter_pf (char *nk) {
          int fcontains (AccPfEntry *e) {
            return str_eq(accPfEntry_nick(e), nk);
          }
          return it_contains(arr_to_it(pf), (FPRED)fcontains);
        }
        nicks = it_filter(nicks, (FPRED)filter_pf);
      }
      map_put(rp, "nicks", arr_to_js(arr_from_it(nicks), (FTO)js_ws));
    }
    asyncActor_wait(ac, fn, NULL);
    return cgi_ok(rp);
  }
  if (str_eq(rq, "setAllCos")) {
    CGI_GET_BOOL(value, mrq, "value")
    void fn (void *null) { conf_set_acc_all_cos(value); }
    asyncActor_wait(ac, (FPROC)fn, NULL);
    return cgi_empty();
  }
  if (str_eq(rq, "historic")) {
    CGI_GET_STR(nick, mrq, "nick")
    void fn (void *null) {
      // Arr[AccEntry]
      Arr *annotations = accdb_diary_read();
      AccLedPf *lp = accLedPf_new(annotations);
      EACH(accLedPf_errors(lp), char, e)
        log_error(e);
      _EACH
      // Arr[AccPfEntry]
      Arr *pf = accLedPf_pf(lp);
      int ffind (AccPfEntry *e) { return str_eq(accPfEntry_nick(e), nick); }
      AccPfEntry *pfe = opt_nget(it_find(arr_to_it(pf), (FPRED)ffind));

      map_put(rp, "url", js_ws(servers_acc_url(nick)));
      RsHistoric *rs = accdb_historic_with_dailyq(nick);

      // Arr[char]
      Arr *rsdates = arr_new();
      // Arr[Js]
      Arr *rsqs = arr_new();
      // Arr[RsChartQ
      It *rss = it_reverse(it_take(
        it_reverse(arr_to_it(rsHistoric_quotes(rs))),
        ACC_CHART_QUOTES
      ));
      EACHI(rss, RsChartQ, qs)
        arr_push(rsdates, rsChartQ_date(qs));
        // Arr[Js]
        Arr *a = arr_new();
        arr_push(a, js_wd(rsChartQ_close(qs)));
        arr_push(a, js_wd(rsChartQ_ref(qs)));
        arr_push(rsqs, js_wa(a));
      _EACH

      map_put(rp, "price", js_wd(pfe ? accPfEntry_price(pfe) : -1));
      map_put(rp, "profits", js_wd(rsHistoric_profits(rs)));
      map_put(rp, "dates", arr_to_js(rsdates, (FTO)js_ws));
      map_put(rp, "quotes", js_wa(rsqs));
      map_put(rp, "historic", arr_to_js(
        rsHistoric_historic(rs), (FTO)rsChartOp_to_js
      ));
    }
    asyncActor_wait(ac, fn, NULL);

    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT("rq", "nicks | setAllCos | historic", rq)
  return NULL; // Unreachable
}


