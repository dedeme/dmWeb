// Copyright 30-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/fleas/wgs/fleas__wgs__wcharts.h"
#include "io/fleasdb.h"
#include "dmc/cgi.h"

// mrq is Map[Js]
char *fleas__wgs__wcharts_process(AsyncActor *ac, Map *mrq) {
  CGI_GET_INT(rq, mrq)
  // Map[Js]
  Map *rp = map_new();

  // From Bests
  if (rq == 0) {
    CGI_GET_STR(model, mrq)
    CGI_GET_STR(nick, mrq)
    void fn () {
      map_put(rp, "data", fleasdb_charts_read_js(model, nick));
    }
    asyncActor_wait(ac, fn);
    return cgi_ok(rp);
  }
  // From champions
  if (rq == 1) {
    CGI_GET_STR(model, mrq)
    CGI_GET_STR(nick, mrq)
    CGI_GET_INT(group, mrq)
    CGI_GET_STR(flea, mrq)
    void fn () {
      map_put(rp, "data", fleasdb_champions_chart_read_js(
        group, model, nick, flea
      ));
    }
    asyncActor_wait(ac, fn);
    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT("rq", "0 | 1", str_f("%d", rq))
  return NULL; // Unreachable
}


