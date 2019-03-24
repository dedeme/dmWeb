// Copyright 27-Feb-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "pages/bests.h"
#include "dmc/cgi.h"
#include "conf.h"
#include "io.h"

// 'mrq' is Map[Js]
void bests_process(Map *mrq) {
  CGI_GET_STR(rq, mrq, "rq")

  if (str_eq(rq, "getModel")) {
    Map *m = map_new(free);
    map_put(m, "model", conf_get_model_new());
    cgi_ok(m);
    map_free(m);
  } else if (str_eq(rq, "setModel")) {
    CGI_GET_STR(model, mrq, "model")
    conf_set_model(model);
    cgi_empty();
    free(model);
  } else if (str_eq(rq, "data")) {
    CGI_GET_STR(model, mrq, "model")
    Map *m = map_new(free);
    map_put(m, "params", io_params_new(model));
    map_put(m, "table", io_bests_new(model));
    cgi_ok(m);
    free(model);
    map_free(m);
  } else {
    FAIL(str_f_new("'%s': Unknown 'rq' in settings", rq))
  }

  free(rq);
}
