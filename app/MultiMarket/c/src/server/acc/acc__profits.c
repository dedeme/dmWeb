// Copyright 27-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/acc/acc__profits.h"
#include "dmc/cgi.h"
#include "dmc/Darr.h"
#include "dmc/date.h"
#include "io/accdb.h"
#include "io/conf.h"
#include "io/log.h"
#include "data/Acc.h"
#include "DEFS.h"


// mrq is Map[Js]
char *acc__profits_process(AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(rq, mrq, "rq")
  // Map[Js]
  Map *rp = map_new();

  if (str_eq(rq, "idata")) {
    void fn () {
      if (str_eq(conf_activity(), ACT_SLEEPING2)) {
        map_put(rp, "data", accdb_profits_read_js());
      } else {
        AccLedPf *rs = accLedPf_new(accdb_diary_read());
        EACH(accLedPf_errors(rs), char, e)
          log_error(e);
        _EACH

        accdb_pf_update(accLedPf_pf(rs));

        time_t now = date_now();
        if (atoi(date_f(now, "%H")) < ACT_HISTORIC_END) {
          now = date_add(now, -1);
        }
        Darr *prfs = accLedPf_profits(rs);
        map_put(rp, "data", accdb_profits_with(
          date_to_str(now),
          darr_get(prfs, 0),
          darr_get(prfs, 1),
          darr_get(prfs, 2)
        ));
      }
    }
    asyncActor_wait(ac, fn);
    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT("rq", "download", rq)
  return NULL; // Unreachable
}

