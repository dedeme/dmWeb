// Copyright 06-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/sys/sys__nicks.h"
#include "dmc/cgi.h"
#include "io/conf.h"
#include "io/nicks.h"
#include "io/quotes.h"
#include "io/net.h"

// All Map[Js]
static Map *download (void *actor, Map *mrq) {
  AsyncActor *ac = actor;
  CGI_GET_INT(nkId, mrq)
  EMsg *e = net_update_historic(ac, nkId);
  Map *rp = map_new();
  map_put(rp, "err", js_wi(eMsg_error(e)));
  map_put(rp, "msg", js_ws(eMsg_msg(e)));
  return rp;
}

// mrq is Map[Js]
char *sys__nicks_process(AsyncActor *ac, Map *mrq) {
  // Map[Js]
  Map *rp = map_new();
  CGI_GET_STR(rq, mrq)

  if (str_eq(rq, "idata")) {
    void fn () {
      // Arr [Nick]
      Arr *list = nicks_list();
      map_put(rp, "nickList", arr_to_js(list, (FTO)nick_to_js));
      map_put(rp, "volume", quotes_volume());
      map_put(rp, "model", js_wi(nicks_model()));
      int nickSelId = conf_nick_sel_id();
      if (nickSelId >= 0) {
        int sel = nickSelId;
        nickSelId = -1;
        EACH(list, Nick, nk)
          if (nick_id(nk) == sel) {
            nickSelId = sel;
            break;
          }
        _EACH
      }
      map_put(rp, "nickSelId", js_wi(nickSelId));
    }
    asyncActor_wait(ac, fn);
    return cgi_ok(rp);
  }

  if (str_eq(rq, "newNick")) {
    CGI_GET_STR(nick, mrq)
    void fn () { map_put(rp, "ok", js_wb(nicks_add(nick))); }
    asyncActor_wait(ac, fn);
    return cgi_ok(rp);
  }

  if (str_eq(rq, "setNickSelId")) {
    CGI_GET_INT(id, mrq)
    void fn () { conf_set_nick_sel_id(id); }
    asyncActor_wait(ac, fn);
    return cgi_empty();
  }

  if (str_eq(rq, "modNick")) {
    CGI_GET_INT(nickId, mrq)
    CGI_GET_STR(nickName, mrq)
    void fn () {
      int ok = 0;
      // Opt[Nick]
      Opt *onk = nicks_get(nickId);
      if (opt_is_full(onk)) {
        Nick *nk = opt_get(onk);
        nick_set_name(nk, nickName);
        ok = nicks_modify(nk);
      }
      map_put(rp, "ok", js_wb(ok));
    }
    asyncActor_wait(ac, fn);
    return cgi_ok(rp);
  }

  if (str_eq(rq, "download")) {
    rp = cgi_long_run(download, ac, mrq);
    return cgi_ok(rp);
  }

  if (str_eq(rq, "test")) {
    CGI_GET_INT(nickId, mrq)
    void fn () {
      EMsg *e = tp_e1(quotes_editor_read(nickId));
      map_put(rp, "err", js_wi(eMsg_error(e)));
      map_put(rp, "msg", js_ws(eMsg_msg(e)));
    }
    asyncActor_wait(ac, fn);
    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT(
    "rq",
    "idata | newNick | newNick | setNickSelId | modNick | download | test",
    rq
  )
  return NULL; // Unreachable
}
