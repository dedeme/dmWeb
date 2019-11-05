// Copyright 27-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "pages/leagues.h"
#include "dmc/cgi.h"
#include "io/leaguesdb.h"

char *leagues_process(Map *mrq) {
  CGI_GET_STR(rq, mrq)

  // Map<Js>
  Map *m = map_new();

  if (str_eq(rq, "sessions")) {
    map_put(m, "sessions", arr_to_js(leaguesdb_sessions(), (FTO)js_ws));
    return cgi_ok(m);
  }

  EXC_ILLEGAL_ARGUMENT(
    "rq",
    "sessions",
    rq
  )
  return NULL; // Unreachable
}


