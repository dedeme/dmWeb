// Copyright 05-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/sys/sys__home.h"
#include "dmc/cgi.h"
#include "io/log.h"

// mrq is Map[Js]
char *sys__home_process(Map *mrq) {
  CGI_GET_STR(rq, mrq, "rq")
  // Map[Js]
  Map *rp = map_new();

  if (str_eq(rq, "getLog")) {
    map_put(rp, "log", log_to_js());
    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT("rq", "getLog", rq)
  return NULL; // Unreachable
}
