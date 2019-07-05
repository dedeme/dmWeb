// Copyright 28-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/daily.h"
#include "dmc/cgi.h"
#include "io/conf.h"
#include "io/sbox.h"
#include "io/dailydb.h"
#include "data/Server.h"

// mrq is Map[Js]
char *daily_process(AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(rq, mrq, "rq")
  // Map[Js]
  Map *rp = map_new();

  if (str_eq(rq, "idata")) {
    void fn (void *null) {
      map_put(rp, "state", js_ws(conf_activity()));
      Server *sv = opt_oget(sbox_get(), NULL);
      map_put(rp, "server", js_ws(sv ? server_name(sv) : "No set"));
      map_put(rp, "cos", dailydb_cos());
    }
    asyncActor_wait(ac, fn, NULL);
    return cgi_ok(rp);
  }
  if (str_eq(rq, "update")) {
    void fn (void *null) {
      dailydb_update_charts();
    }
    asyncActor_wait(ac, fn, NULL);
    return cgi_empty();
  }

  EXC_ILLEGAL_ARGUMENT("rq", "idata", rq)
  return NULL; // Unreachable
}



