// Copyright 04-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/hub.h"
#include "dmc/cgi.h"
#include "dmc/cryp.h"
#include "DEFS.h"
#include "io/backups.h"
#include "io/conf.h"
#include "server/sys.h"
#include "server/fleas.h"
#include "server/acc.h"
#include "server/daily.h"
#include "server/ranking.h"

// rq is Map[Js]
static char *module_process(AsyncActor *ac, char *module, Map *mrq) {
  // Map[Js]
  Map *rp = map_new();
  if (str_eq(module, ".")) {
    void fn() { map_put(rp, "lang", js_ws(conf_lang())); }
    asyncActor_wait(ac, fn);
    return cgi_ok(rp);
  }
  if (str_eq(module, "sys")) return sys_process(ac, mrq);
  if (str_eq(module, "fleas")) return fleas_process(ac, mrq);
  if (str_eq(module, "acc")) return acc_process(ac, mrq);
  if (str_eq(module, "daily")) return daily_process(ac, mrq);
  if (str_eq(module, "ranking")) return ranking_process(ac, mrq);

  EXC_ILLEGAL_ARGUMENT("module", "sys | fleas", module)
  return NULL; // Unreachable
}

char *hub_rp (AsyncActor *ac, char *rq) {
  int ix = str_cindex(rq, ':');
  //............................................................. CONNECTION
  if (ix == -1) {
    cgi_set_key(rq);
    return cgi_connect(rq);
  }

  //......................................................... AUTHENTICATION
  if (ix == 0) {
    char *key = cryp_key(APP_NAME, cgi_klen());
    cgi_set_key(key);
    char *data = cryp_decryp(rq + 1, key);
    // Arr[char]
    Arr *parts = str_csplit(data, ':');
    return cgi_authentication(
      arr_get(parts, 0),
      arr_get(parts, 1),
      *((char *)arr_get(parts, 2)) == '1'
    );
  }

  //............................................................ NORMAL DATA
  char *session_id = str_left(rq, ix);
  // Opt [CgiSession]
  Opt *ossdata = cgi_get_session(session_id);

  if (opt_is_empty(ossdata)) {
    cgi_set_key("nosession");
    return cgi_expired();
  }

  CgiSession *ssdata = opt_get(ossdata);

  char *key = cgiSession_key(ssdata);
  cgi_set_key(key);
  char *data = cryp_decryp(str_right(rq, ix + 1), key);

  // Map[Js]
  Map *m = js_ro((Js *)data);
  // Opt[Js]
  Opt *conn_id_js = map_get(m, "connectionId");
  if (opt_is_full(conn_id_js)) {
    char *conn_id = js_rs(opt_get(conn_id_js));
    if (!str_eq(conn_id, cgiSession_id(ssdata))) {
      cgi_set_key("nosession");
      return cgi_expired();
    }
  }

  //..................................... logout
  CGI_GET_STR(module, m);

  if (str_eq(module, "logout")) {
    backups_make_automatic();
    return cgi_del_session(session_id);
  }

  return module_process(ac, module, m);
}
