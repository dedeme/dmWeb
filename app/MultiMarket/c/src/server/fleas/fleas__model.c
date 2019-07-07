// Copyright 21-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/fleas/fleas__model.h"
#include "io/fleasdb.h"
#include "data/dfleas/dfleas__models.h"
#include "data/Model.h"
#include "dmc/cgi.h"

// mrq is Map[Js]
char *fleas__model_process(AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(rq, mrq, "rq")
  // Map[Js]
  Map *rp = map_new();

  if (str_eq(rq, "getDates")) {
    CGI_GET_STR(model, mrq, "model");
    void fn (void *null) {
      map_put(rp, "dates", arr_to_js(fleasdb_model_dates(model), (FTO)js_ws));
    }
    asyncActor_wait(ac, fn, NULL);
    return cgi_ok(rp);
  }
  if (str_eq(rq, "data")) {
    CGI_GET_STR(model, mrq, "model");
    CGI_GET_STR(date, mrq, "date");
    // Opt[Model]
    Opt *md = dfleas__models_get(model);
    if (opt_is_empty(md)) {
      map_put(rp, "params", js_wa(arr_new()));
      map_put(rp, "table", js_wa(arr_new()));
    } else {
      Model *m = opt_get(md);
      // Arr[Js]
      Arr *params = arr_new();
      arr_push(params, arr_to_js(model_param_names(m), (FTO)js_ws));
      arr_push(params, model_param_jss(m));
      map_put(rp, "params", js_wa(params));
      void fn (void *null) {
        map_put(rp, "table", fleasdb_model_read_js(model_name(m), date));
      }
      asyncActor_wait(ac, fn, NULL);
    }
    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT("rq", "getModel | setModel | nicks | data", rq)
  return NULL; // Unreachable
}


