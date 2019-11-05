// Copyright 26-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "pages/pgmain.h"
#include "dmc/cgi.h"
#include "io/conf.h"

char *pgmain_process(Map *mrq) {
  CGI_GET_STR(rq, mrq)

  // Map<Js>
  Map *m = map_new();

  if (str_eq(rq, "lang")) {
    map_put(m, "lang", js_ws(conf_lang()));
    return cgi_ok(m);
  }

  EXC_ILLEGAL_ARGUMENT("rq", "lang", rq)
  return NULL; // Unreachable
}
