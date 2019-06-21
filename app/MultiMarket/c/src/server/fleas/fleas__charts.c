// Copyright 19-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/fleas/fleas__charts.h"
#include "io.h"
#include "io/conf.h"
#include "io/fleasdb.h"
#include "scheduler/fleas/fleas__models.h"
#include "data/Model.h"
#include "dmc/cgi.h"

// mrq is Map[Js]
char *fleas__charts_process(AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(rq, mrq, "rq")
  // Map[Js]
  Map *rp = map_new();

  if (str_eq(rq, "getModel")) {
    void fn (void *null) {
      map_put(rp, "model", js_ws(conf_fleas_model()));
    }
    asyncActor_wait(ac, fn, NULL);
    return cgi_ok(rp);
  }
  if (str_eq(rq, "setModel")) {
    CGI_GET_STR(model, mrq, "model")
    void fn (void *null) { conf_set_fleas_model(model); }
    asyncActor_wait(ac, fn, NULL);
    return cgi_empty();
  }
  if (str_eq(rq, "nicks")) {
    CGI_GET_STR(model, mrq, "model")
    void fn (void *null) {
      map_put(rp, "list", fleasdb_charts_read_nicks_js(model));
    }
    asyncActor_wait(ac, fn, NULL);
    return cgi_ok(rp);
  }
  if (str_eq(rq, "data")) {
    CGI_GET_STR(model, mrq, "model");
    CGI_GET_STR(nick, mrq, "nick");
    map_put(rp, "data", fleasdb_charts_read_js(model, nick));
    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT("rq", "getModel | setModel | nicks | data", rq)
  return NULL; // Unreachable
}

