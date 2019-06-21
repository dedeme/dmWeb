// Copyright 19-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/fleas/fleas__bests.h"
#include "io.h"
#include "io/conf.h"
#include "io/fleasdb.h"
#include "scheduler/fleas/fleas__models.h"
#include "data/Model.h"
#include "dmc/cgi.h"

// mrq is Map[Js]
char *fleas__bests_process(AsyncActor *ac, Map *mrq) {
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
  if (str_eq(rq, "data")) {
    CGI_GET_STR(model, mrq, "model");
    // Opt[Model]
    Opt *md = fleas__models_get(model);
    if (opt_is_empty(md)) {
      map_put(rp, "params", js_wa(arr_new()));
      map_put(rp, "table", js_wa(arr_new()));
    } else {
      void fn (void *null) {
        Model *m = opt_get(md);
        // Arr[Js]
        Arr *params = arr_new();
        arr_push(params, arr_to_js(model_param_names(m), (FTO)js_ws));
        arr_push(params, model_param_jss(m));
        map_put(rp, "params", js_wa(params));
        map_put(rp, "table", fleasdb_bests_read_js(model));
      }
      asyncActor_wait(ac, fn, NULL);
    }
    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT("rq", "getModel | setModel | data", rq)
  return NULL; // Unreachable
}

