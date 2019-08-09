// Copyright 05-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/sys/sys__main.h"
#include "io/conf.h"
#include "dmc/cgi.h"

// mrq is Map[Js]
char *sys__main_process(AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(rq, mrq, "rq")
  // Map[Js]
  Map *rp = map_new();

  if (str_eq(rq, "idata")) {
    void fn () { map_put(rp, "page", js_ws(conf_sys_page())); }
    asyncActor_wait(ac, fn);
    return cgi_ok(rp);
  }
  if (str_eq(rq, "go")) {
    CGI_GET_STR(option, mrq, "option")
    void fn () { conf_set_sys_page(option); }
    asyncActor_wait(ac, fn);
    return cgi_empty();
  }

  EXC_ILLEGAL_ARGUMENT("rq", "idata | go", rq)
  return NULL; // Unreachable
}
