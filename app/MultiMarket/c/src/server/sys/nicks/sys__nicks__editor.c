// Copyright 31-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/sys/nicks/sys__nicks__editor.h"
#include "dmc/cgi.h"
#include "io/nicks.h"
#include "io/quotes.h"
#include "io/servers.h"
#include "io/log.h"
#include "data/Quote.h"
#include "io.h"
#include "DEFS.h"

/// mrq is Map[Js]
char *sys__nicks__editor_process(AsyncActor *ac, Map *mrq) {
// Map[Js]
  Map *rp = map_new();
  CGI_GET_STR(rq, mrq, "rq")

  if (str_eq(rq, "nick")) {
    CGI_GET_INT(id, mrq, "id")
    void fn (void *null) {
      // Opt[Nick]
      Opt *nick = nicks_get(id);
      map_put(rp, "nick", opt_is_empty(nick)
        ? js_wn()
        : nick_to_js(opt_get(nick))
      );
    }
    asyncActor_wait(ac, fn, NULL);
    return cgi_ok(rp);
  }

  if (str_eq(rq, "quotes")) {
    CGI_GET_STR(name, mrq, "name")
    void fn (void *null) {
      map_put(rp, "quotes", arr_to_js(quotes_read(name), (FTO)quote_to_js));
    }
    asyncActor_wait(ac, fn, NULL);
    return cgi_ok(rp);
  }

  if (str_eq(rq, "quotes2")) {
    CGI_GET_INT(nick_id, mrq, "nickId")
    CGI_GET_INT(nick_id2, mrq, "nickId2")
    void fn (void *null) {
      // Tp[EMsg, char]
      Tp *e_s1 = quotes_editor_read(nick_id);
      EMsg *emsg = tp_e1(e_s1);
      map_put(rp, "err", js_wi(eMsg_error(emsg)));
      map_put(rp, "msg", js_ws(eMsg_msg(emsg)));
      map_put(rp, "qs1", js_ws(tp_e2(e_s1)));
      // Tp[EMsg, char]
      Tp *e_s2 = quotes_editor_read(nick_id2);
      emsg = tp_e1(e_s2);
      map_put(rp, "qs2", js_ws(eMsg_error(emsg) == MSG_OK ? tp_e2(e_s2) : ""));
      map_put(rp, "nicks", arr_to_js(nicks_list(), (FTO)nick_to_js));
    }
    asyncActor_wait(ac, fn, NULL);
    return cgi_ok(rp);
  }

  if (str_eq(rq, "serverCodes")) {
    CGI_GET_INT(id, mrq, "id")
    void fn (void *null) {
      map_put(rp, "sIdNameCodes",
        arr_to_js(servers_nick_codes(id), (FTO)serversIdNameCode_to_js)
      );
    }
    asyncActor_wait(ac, fn, NULL);
    return cgi_ok(rp);
  }

  if (str_eq(rq, "setCode")) {
    CGI_GET_INT(server_id, mrq, "serverId")
    CGI_GET_INT(nick_id, mrq, "nickId")
    CGI_GET_STR(code, mrq, "code")
    void fn (void *null) {
      servers_set_nick_code(server_id, nick_id, code);
    }
    asyncActor_wait(ac, fn, NULL);
    return cgi_empty();
  }

  if (str_eq(rq, "setQuotes")) {
    CGI_GET_INT(nick_id, mrq, "nickId")
    CGI_GET_STR(qs, mrq, "qs")
    void fn (void *null) {
      EMsg *emsg = quotes_editor_set_quotes(nick_id, qs);
      map_put(rp, "err", js_wi(eMsg_error(emsg)));
      map_put(rp, "msg", js_ws(eMsg_msg(emsg)));
    }
    asyncActor_wait(ac, fn, NULL);
    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT(
    "rq",
    "nick | quotes | quotes2 | serverCodes | setCode | setQuotes",
    rq
  )
  return NULL; // Unreachable

}

