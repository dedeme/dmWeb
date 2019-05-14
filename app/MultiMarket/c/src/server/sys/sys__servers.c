// Copyright 11-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/sys/sys__servers.h"
#include "dmc/cgi.h"
#include "io.h"
#include "io/servers.h"

// mrq is Map[Js]
char *sys__servers_process(Map *mrq) {
  // Map[Js]
  Map *rp = map_new();
  CGI_GET_STR(rq, mrq, "rq")

  if (str_eq(rq, "serverList")) {
    void (fn)(void *null) {
      // Arr[Js]
      Arr *list = arr_new();
      EACH(servers_list(), Server, sv)
        arr_push(list, js_wi(server_id(sv)));
      _EACH
      map_put(rp, "serverList", js_wa(list));
    }
    io_wait(fn, NULL);
    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT("rq", "serverList", rq)
  return NULL; // Unreachable
}
