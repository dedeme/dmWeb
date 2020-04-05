// Copyright 23-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/fleas/charts/companies.h"
#include "dmc/cgi.h"

char *companies_process (AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(rq, mrq);

  if (str_eq(rq, "idata")) {
    // Map[Js]
    Map *rp = map_new();
    map_put(rp, "data", js_ws(""));

    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT("rq", "idata", rq)
  return NULL; // Unreachable
}
