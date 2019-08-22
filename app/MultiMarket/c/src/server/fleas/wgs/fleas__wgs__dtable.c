// Copyright 30-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/fleas/wgs/fleas__wgs__dtable.h"
#include "dmc/cgi.h"
#include "data/Nick.h"
#include "io/nicks.h"

// mrq is Map[Js]
char *fleas__wgs__dtable_process(AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(rq, mrq)
  // Map[Js]
  Map *rp = map_new();

  if (str_eq(rq, "nicks")) {
    void fn () {
      int ffilter (Nick *nk) { return nick_is_sel(nk); }
      void *fmap (Nick *nk) { return nick_name(nk); }
      map_put(rp, "nicks", arr_to_js(
        arr_from_it(it_map(
          it_filter(arr_to_it(nicks_list()), (FPRED)ffilter),
          (FCOPY)fmap
        )),
        (FTO)js_ws
      ));
    }
    asyncActor_wait(ac, fn);
    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT("rq", "nicks", rq)
  return NULL; // Unreachable
}


