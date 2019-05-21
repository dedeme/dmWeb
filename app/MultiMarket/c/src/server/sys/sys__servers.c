// Copyright 11-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/sys/sys__servers.h"
#include "dmc/cgi.h"
#include "io.h"
#include "io/servers.h"
#include "io/nicks.h"
#include "io/conf.h"

// mrq is Map[Js]
char *sys__servers_process(Map *mrq) {
  // Map[Js]
  Map *rp = map_new();
  CGI_GET_STR(rq, mrq, "rq")

  if (str_eq(rq, "idata")) {
    void (fn)(void *null) {
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
    io_wait(fn, NULL);
    return cgi_ok(rp);
  }

  if (str_eq(rq, "new")) {
    void (fn)(void *null) {
      CGI_GET_STR(short_name, mrq, "shortName")
      map_put(rp, "ok", js_wb(servers_add(short_name)));
    }
    io_wait(fn, NULL);
    return cgi_ok(rp);
  }

  if (str_eq(rq, "del")) {
    void (fn)(void *null) {
      CGI_GET_INT(id, mrq, "id")
      servers_remove(id);
    }
    io_wait(fn, NULL);
    return cgi_ok(rp);
  }

  if (str_eq(rq, "setServerSelId")) {
    void (fn)(void *null) {
      CGI_GET_INT(id, mrq, "id")
      conf_set_server_sel_id(id);
    }
    io_wait(fn, NULL);
    return cgi_empty();
  }

  if (str_eq(rq, "setServerTab")) {
    void (fn)(void *null) {
      CGI_GET_STR(id, mrq, "id")
      conf_set_server_tab(id);
    }
    io_wait(fn, NULL);
    return cgi_empty();
  }

  if (str_eq(rq, "nicks")) {
    void (fn)(void *null) {
      map_put(rp, "nicks", arr_to_js(nicks_list(), (FTO)nick_to_js));
    }
    io_wait(fn, NULL);
    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT(
    "rq", "idata | new | del | setServerSelId | setServerTab | nicks", rq
  )
  return NULL; // Unreachable
}
