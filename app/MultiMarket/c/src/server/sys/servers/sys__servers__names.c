// Copyright 20-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/sys/servers/sys__servers__names.h"
#include "dmc/cgi.h"
#include "io/servers.h"

char *sys__servers__names_process(AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(rq, mrq)
  // Map[Js]
  Map *rp = map_new();

  if (str_eq(rq, "changeNames")) {
    CGI_GET_INT(id, mrq)
    CGI_GET_STR(shortName, mrq)
    CGI_GET_STR(name, mrq)
    void fn () {
      map_put(rp, "ok", js_wb(servers_set_names(id, shortName, name)));
    }
    asyncActor_wait(ac, fn);
    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT("rq", "changeNames", rq)
  return NULL; // Unreachable

}
