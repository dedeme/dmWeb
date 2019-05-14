// Copyright 06-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/sys/sys__nicks.h"
#include "dmc/cgi.h"
#include "io.h"
#include "io/conf.h"
#include "io/nicks.h"

// mrq is Map[Js]
char *sys__nicks_process(Map *mrq) {
  // Map[Js]
  Map *rp = map_new();
  CGI_GET_STR(rq, mrq, "rq")

  if (str_eq(rq, "idata")) {
    void (fn)(void *null) {
      // Arr [Nick]
      Arr *list = nicks_list();
      map_put(rp, "nickList", arr_to_js(list, (FTO)nick_to_js));
      map_put(rp, "model", js_wi(nicks_model()));
      int sel = conf_nick_sel_id();
      int nickSelId = -1;
      EACH(list, Nick, nk)
        if (nick_id(nk) == sel) {
          nickSelId = sel;
          break;
        }
      _EACH
      map_put(rp, "nickSelId", js_wi(nickSelId));
    }
    io_wait(fn, NULL);
    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT("rq", "idata", rq)
  return NULL; // Unreachable
}
