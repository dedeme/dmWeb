// Copyright 22-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/sys/servers/sys__servers__download.h"
#include "dmc/cgi.h"
#include "io/servers.h"
#include "data/Rconf.h"
#include "io/log.h"

// rq and return are Map[Js]
static Map *test_daily_conf(void *null, Map *rq) {
  CGI_GET_INT(server_id, rq, "serverId")
  // Map[Js]
  Map *rp = map_new();
  TRY
    map_put(rp, "ok", js_wb(servers_test_daily_conf(server_id)));
  CATCH (ex)
    log_exception(ex);
    map_put(rp, "ok", js_wb(0));
  _TRY
  return rp;
}

// rq and return are Map[Js]
static Map *test_historic_conf(void *null, Map *rq) {
  CGI_GET_INT(server_id, rq, "serverId")
  CGI_GET_INT(nick_id, rq, "nickId")
  // Map[Js]
  Map *rp = map_new();
  TRY
    map_put(rp, "ok", js_wb(servers_test_historic_conf(server_id, nick_id)));
  CATCH (ex)
    log_exception(ex);
    map_put(rp, "ok", js_wb(0));
  _TRY
  return rp;
}

char *sys__servers__download_process(AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(rq, mrq, "rq")
  // Map[Js]
  Map *rp = map_new();

  if (str_eq(rq, "activate")) {
    CGI_GET_INT(id, mrq, "id")
    CGI_GET_BOOL(historic, mrq, "historic")
    CGI_GET(Rconf *, conf, rconf_from_js, mrq, "conf")
    void fn () { servers_activate(id, historic, conf); }
    asyncActor_wait(ac, fn);
    return cgi_empty();
  }
  if (str_eq(rq, "modify")) {
    CGI_GET_INT(id, mrq, "id")
    CGI_GET_BOOL(historic, mrq, "historic")
    CGI_GET(Rconf *, conf, rconf_from_js, mrq, "conf")
    void fn () { servers_set_conf(id, historic, conf); }
    asyncActor_wait(ac, fn);
    return cgi_empty();
  }
  if (str_eq(rq, "dailyTest")) {
    void fn () { rp = cgi_long_run(test_daily_conf, NULL, mrq); }
    asyncActor_wait(ac, fn);
    return cgi_ok(rp);
  }
  if (str_eq(rq, "historicTest")) {
    void fn () { rp = cgi_long_run(test_historic_conf, NULL, mrq); }
    asyncActor_wait(ac, fn);
    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT("rq", "activate | modify", rq)
  return NULL; // Unreachable

}

