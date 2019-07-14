// Copyright 26-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/acc/acc__balance.h"
#include "dmc/cgi.h"
#include "io/accdb.h"
#include "io/log.h"
#include "io/managerdb.h"
#include "data/Acc.h"

// mrq is Map[Js]
char *acc__balance_process(AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(rq, mrq, "rq")
  // Map[Js]
  Map *rp = map_new();

  if (str_eq(rq, "idata")) {
    void fn (void *null) {
      AccLedPf *rs = accLedPf_new(accdb_diary_read());
      EACH(accLedPf_errors(rs), char, e)
        log_error(e);
      _EACH
      AccLedger *ledger = accLedPf_ledger(rs);
      map_put(rp, "ledger", accLedger_to_js(ledger));
      // Arr[AccPfEntry]
      Arr *pf = accLedPf_pf(rs);
      accdb_pf_update(pf);
      map_put(rp, "pf", arr_to_js(pf, (FTO)accPfEntry_to_js));
      ModelParams *mps = managerdb_default();
      map_put(rp, "inc", js_wd(darr_get(modelParams_params(mps), 0)));
    }
    asyncActor_wait(ac, fn, NULL);
    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT("rq", "idata", rq)
  return NULL; // Unreachable
}

