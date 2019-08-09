// Copyright 19-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/fleas/fleas__bests.h"
#include "io/conf.h"
#include "io/fleasdb.h"
#include "data/dfleas/dfleas__models.h"
#include "data/Model.h"
#include "data/ModelMxMn.h"
#include "dmc/cgi.h"

// mrq is Map[Js]
char *fleas__bests_process(AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(rq, mrq, "rq")
  // Map[Js]
  Map *rp = map_new();

  if (str_eq(rq, "getModel")) {
    void fn () {
      map_put(rp, "model", js_ws(conf_fleas_model()));
    }
    asyncActor_wait(ac, fn);
    return cgi_ok(rp);
  }
  if (str_eq(rq, "setModel")) {
    CGI_GET_STR(model, mrq, "model")
    void fn () { conf_set_fleas_model(model); }
    asyncActor_wait(ac, fn);
    return cgi_empty();
  }
  if (str_eq(rq, "data")) {
    CGI_GET_STR(model, mrq, "model");
    // Opt[Model]
    Opt *md = dfleas__models_get(model);
    if (opt_is_empty(md)) {
      map_put(rp, "params", js_wa(arr_new()));
      map_put(rp, "table", js_wa(arr_new()));
    } else {
      void fn () {
        Model *m = opt_get(md);
        // Arr[Js]
        Arr *params = arr_new();
        arr_push(params, arr_to_js(
          modelMxMn_names(model_param_cf(m)), (FTO)js_ws
        ));
        arr_push(params, model_param_jss(m));
        map_put(rp, "params", js_wa(params));
        map_put(rp, "table", fleasdb_bests_read_js(model));
      }
      asyncActor_wait(ac, fn);
    }
    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT("rq", "getModel | setModel | data", rq)
  return NULL; // Unreachable
}

