// Copyright 11-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/sys/sys__servers.h"
#include "dmc/cgi.h"
#include "io/servers.h"
#include "io/nicks.h"
#include "io/conf.h"

// mrq is Map[Js]
char *sys__servers_process(AsyncActor *ac, Map *mrq) {
  // Map[Js]
  Map *rp = map_new();
  CGI_GET_STR(rq, mrq, "rq")

  if (str_eq(rq, "idata")) {
    void (fn)() {
      // Arr[Server]
      Arr *svs = servers_list();
      map_put(rp, "serverList", arr_to_js(svs, (FTO)server_to_js));
      int sel = conf_server_sel_id();
      int serverSelId = -1;
      EACH(svs, Server, sv)
        if (server_id(sv) == sel) {
          serverSelId = sel;
          break;
        }
      _EACH
      map_put(rp, "serverSelId", js_wi(serverSelId));
      map_put(rp, "serverTab", js_ws(conf_server_tab()));
    }
    asyncActor_wait(ac, fn);
    return cgi_ok(rp);
  }

  if (str_eq(rq, "new")) {
    CGI_GET_STR(short_name, mrq, "shortName")
    void (fn)() { map_put(rp, "ok", js_wb(servers_add(short_name))); }
    asyncActor_wait(ac, fn);
    return cgi_ok(rp);
  }

  if (str_eq(rq, "del")) {
    CGI_GET_INT(id, mrq, "id")
    void (fn)() { servers_remove(id); }
    asyncActor_wait(ac, fn);
    return cgi_ok(rp);
  }

  if (str_eq(rq, "setServerSelId")) {
    CGI_GET_INT(id, mrq, "id")
    void (fn)() { conf_set_server_sel_id(id); }
    asyncActor_wait(ac, fn);
    return cgi_empty();
  }

  if (str_eq(rq, "setServerTab")) {
    CGI_GET_STR(id, mrq, "id")
    void (fn)() { conf_set_server_tab(id); }
    asyncActor_wait(ac, fn);
    return cgi_empty();
  }

  if (str_eq(rq, "nicks")) {
    void (fn)() {
      map_put(rp, "nicks", arr_to_js(nicks_list(), (FTO)nick_to_js));
    }
    asyncActor_wait(ac, fn);
    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT(
    "rq", "idata | new | del | setServerSelId | setServerTab | nicks", rq
  )
  return NULL; // Unreachable
}
