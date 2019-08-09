// Copyright 05-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/sys/sys__home.h"
#include "dmc/cgi.h"
#include "io/log.h"
#include "io/conf.h"
#include "io/fleasdb.h"
#include "scheduler/fleas.h"

// mrq is Map[Js]
char *sys__home_process(AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(rq, mrq, "rq")
  // Map[Js]
  Map *rp = map_new();

  if (str_eq(rq, "getLog")) {
    void fn () { map_put(rp, "log", log_to_js()); }
    asyncActor_wait(ac, fn);
    return cgi_ok(rp);
  }

  if (str_eq(rq, "clearLog")) {
    void fn () { log_clear(); }
    asyncActor_wait(ac, fn);
    return cgi_empty();
  }

  if (str_eq(rq, "runFleas")) {
    fleas_run(ac);
    return cgi_empty();
  }

  if (str_eq(rq, "stopFleas")) {
    void fn () { conf_set_fleas_running(0); }
    asyncActor_wait(ac, fn);
    return cgi_empty();
  }

  if (str_eq(rq, "logFleas")) {
    void fn () { map_put(rp, "log", fleasdb_flog_to_js()); }
    asyncActor_wait(ac, fn);
    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT(
    "rq",
    "getLog | clearLog | runFleas | stopFleas | logFleas",
    rq
  )
  return NULL; // Unreachable
}
