// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/home/main.h"
#include "dmc/cgi.h"
#include "db/log.h"

char *mainHome_process (AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(rq, mrq);

  if (str_eq(rq, "log")) {
    Map *rp = map_new();
    map_put(rp, "log", log_read());
    return cgi_ok(rp);
  }
  if (str_eq(rq, "reset")) {
    void fn () { log_reset(); }
    asyncActor_wait(ac, fn);
    return cgi_empty();
  }

  EXC_ILLEGAL_ARGUMENT("rq", "log | reset", rq)
  return NULL; // Unreachable
}
