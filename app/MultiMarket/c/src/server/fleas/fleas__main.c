// Copyright 19-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/fleas/fleas__main.h"
#include "io/conf.h"
#include "data/Model.h"
#include "data/ModelMxMn.h"
#include "data/dfleas/dfleas__models.h"
#include "dmc/cgi.h"

// mrq is Map[Js]
char *fleas__main_process(AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(rq, mrq)
  // Map[Js]
  Map *rp = map_new();

  if (str_eq(rq, "idata")) {
    void fn () {
      // Map[Js]
      Map *pnames = map_new();
      EACH(dfleas__models(), Model, md)
        // Arr[Js]
        Arr *params = arr_new();
        arr_push(params, arr_to_js(
          modelMxMn_names(model_param_cf(md)), (FTO)js_ws
        ));
        arr_push(params, model_param_jss(md));

        map_put(pnames, model_name(md), js_wa(params));
      _EACH
      map_put(rp, "pnames", js_wo(pnames));
    }
    asyncActor_wait(ac, fn);
    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT("rq", "idata", rq)
  return NULL; // Unreachable
}
