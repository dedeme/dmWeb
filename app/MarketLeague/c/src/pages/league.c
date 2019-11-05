// Copyright 30-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "pages/league.h"
#include "dmc/cgi.h"
#include "io/leaguesdb.h"

char *league_process(Map *mrq) {
  CGI_GET_STR(rq, mrq)

  // Map<Js>
  Map *m = map_new();

  if (str_eq(rq, "rounds")) {
    CGI_GET_STR(session, mrq)
    map_put(m, "rounds", arr_to_js(leaguesdb_rounds(session), (FTO)js_ws));
    return cgi_ok(m);
  }

  EXC_ILLEGAL_ARGUMENT(
    "rq",
    "rounds",
    rq
  )
  return NULL; // Unreachable
}

