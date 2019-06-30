// Copyright 30-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/fleas/wgs/fleas__wgs__wcharts.h"
#include "io/fleasdb.h"
#include "dmc/cgi.h"

// mrq is Map[Js]
char *fleas__wgs__wcharts_process(AsyncActor *ac, Map *mrq) {
  CGI_GET_INT(rq, mrq, "rq")
  // Map[Js]
  Map *rp = map_new();

  // From Bests
  if (rq == 0) {
    CGI_GET_STR(model, mrq, "model")
    CGI_GET_STR(nick, mrq, "nick")
    void fn (void *null) {
      map_put(rp, "data", fleasdb_charts_read_js(model, nick));
    }
    asyncActor_wait(ac, fn, NULL);
    return cgi_ok(rp);
  }
  // From champions
  if (rq == 1) {
    CGI_GET_STR(model, mrq, "model")
    CGI_GET_STR(nick, mrq, "nick")
    CGI_GET_INT(group, mrq, "group")
    CGI_GET_STR(flea, mrq, "flea")
    void fn (void *null) {
      map_put(rp, "data", fleasdb_champions_chart_read_js(
        group, model, nick, flea
      ));
    }
    asyncActor_wait(ac, fn, NULL);
    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT("rq", "0 | 1", str_f("%d", rq))
  return NULL; // Unreachable
}


