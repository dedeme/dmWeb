// Copyright 19-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/fleas/fleas__main.h"
#include "io.h"
#include "io/conf.h"
#include "scheduler/fleas/fleas__models.h"
#include "dmc/cgi.h"

// mrq is Map[Js]
char *fleas__main_process(AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(rq, mrq, "rq")
  // Map[Js]
  Map *rp = map_new();

  if (str_eq(rq, "idata")) {
    void fn (void *null) {
      map_put(rp, "page", js_ws(conf_fleas_page()));
      map_put(rp, "models", arr_to_js(fleas__models_names(), (FTO)js_ws));
    }
    asyncActor_wait(ac, fn, NULL);
    return cgi_ok(rp);
  }
  if (str_eq(rq, "go")) {
    CGI_GET_STR(option, mrq, "option")
    asyncActor_wait(ac, (FPROC)conf_set_fleas_page, option);
    return cgi_empty();
  }

  EXC_ILLEGAL_ARGUMENT("rq", "idata | go", rq)
  return NULL; // Unreachable
}
