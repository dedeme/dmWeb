// Copyright 21-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/fleas/fleas__champions.h"
#include "io/conf.h"
#include "io/fleasdb.h"
#include "data/dfleas/dfleas__models.h"
#include "data/Model.h"
#include "dmc/cgi.h"

// mrq is Map[Js]
char *fleas__champions_process(AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(rq, mrq)
  // Map[Js]
  Map *rp = map_new();

  if (str_eq(rq, "getNParams")) {
    void fn () {
      int nparams = conf_champions_nparams();
      map_put(rp, "nparams", nparams == -1 ? js_wn() : js_wi(nparams));
    }
    asyncActor_wait(ac, fn);
    return cgi_ok(rp);
  }
  if (str_eq(rq, "setNParams")) {
    CGI_GET_INT(nparams, mrq)
    void fn () { conf_set_champions_nparams(nparams); }
    asyncActor_wait(ac, fn);
    return cgi_empty();
  }
  if (str_eq(rq, "data")) {
    CGI_GET_INT(nparams, mrq)
    void fn () {
      map_put(rp, "table", fleasdb_champions_read_js(nparams));
    }
    asyncActor_wait(ac, fn);
    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT("rq", "getNParams | setNParams | data", rq)
  return NULL; // Unreachable
}


