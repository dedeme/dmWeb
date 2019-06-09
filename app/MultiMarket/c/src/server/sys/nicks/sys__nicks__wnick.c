// Copyright 16-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/sys/nicks/sys__nicks__wnick.h"
#include "dmc/cgi.h"
#include "io/nicks.h"
#include "io.h"

/// mrq is Map[Js]
char *sys__nicks__wnick_process(AsyncActor *ac, Map *mrq) {
// Map[Js]
//  Map *rp = map_new();
  CGI_GET_STR(rq, mrq, "rq")

  if (str_eq(rq, "setIsSel")) {
    CGI_GET_INT(id, mrq, "id")
    CGI_GET_BOOL(value, mrq, "value")
    void fn (void *null) { nicks_set_selected(id, value); }
    asyncActor_wait(ac, fn, NULL);
    return cgi_empty();
  }

  if (str_eq(rq, "setModel")) {
    CGI_GET_INT(id, mrq, "id")
    void fn (void *null) { nicks_set_model(id); }
    asyncActor_wait(ac, fn, NULL);
    return cgi_empty();
  }

  if (str_eq(rq, "del")) {
    CGI_GET_INT(id, mrq, "id")
    void fn (void *null) { nicks_del(id); }
    asyncActor_wait(ac, fn, NULL);
    return cgi_empty();
  }

  EXC_ILLEGAL_ARGUMENT("rq", "setIsSel | setModel | del", rq)
  return NULL; // Unreachable

}
