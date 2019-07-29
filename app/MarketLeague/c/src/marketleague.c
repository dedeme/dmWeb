// Copyright 17-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "marketleague.h"
#include "dmc/cgi.h"
#include "dmc/cryp.h"
#include "io.h"
#include "Data.h"

static char *app_name = "MarketLeague";
// static char *data_version = "201907";
static char *app_dir = "dmcgi/MarketLeague";
static time_t expiration = 3600;

// mrq is Map[Js]
static char *process(char *session_id, Map *mrq) {
  CGI_GET_STR(rq, mrq, "rq")

  // Map[Js]
  Map *rp = map_new();

  if (str_eq(rq, "getLang")) {
    map_put(rp, "lang", js_ws(io_lang()));
    return cgi_ok(rp);
  }

  if (str_eq(rq, "setLang")) {
    CGI_GET_STR(lang, mrq, "lang");
    io_set_lang(lang);
    cgi_empty();
  }

  if (str_eq(rq, "chpass")) {
    CGI_GET_STR(user, mrq, "user");
    CGI_GET_STR(pass, mrq, "pass");
    CGI_GET_STR(new_pass, mrq, "newPass");
    return cgi_change_pass(user, pass, new_pass);
  }

  if (str_eq(rq, "logout")) {
    return cgi_del_session(session_id);
  }

  if (str_eq(rq, "init")) {
    map_put(rp, "data", dataAll_to_js(io_init()));
    return cgi_ok(rp);
  }

  if (str_eq(rq, "update")) {
    CGI_GET(DataAll *, data, dataAll_from_js, mrq, "data");
    map_put(rp, "data", dataAll_to_js(io_update(data)));
    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT(
    "rq",
    "getLang | setLang | chpass | logout | init | update",
    rq
  )
  return NULL; // Unreachable
}

int main (int argc, char *argv[]) {
  if (argc != 2)
    EXC_ILLEGAL_STATE("argc must be 2")

  cgi_init(app_dir, expiration);

  char *rq = argv[1];
  int ix = str_cindex(rq, ':');
  //............................................................. CONNECTION
  if (ix == -1) {
    cgi_set_key(rq);
    puts(cgi_connect(rq));

    return 0;
  }
  //......................................................... AUTHENTICATION
  if (ix == 0) {
    char *key = cryp_key(app_name, cgi_klen());
    cgi_set_key(key);
    char *data = cryp_decryp(rq + 1, key);

    // Arr[char]
    Arr *parts = str_csplit(data, ':');
    puts(cgi_authentication(
      arr_get(parts, 0),
      arr_get(parts, 1),
      *((char *)arr_get(parts, 2)) == '1'
    ));

    return 0;
  }
  //............................................................ NORMAL DATA
  char *session_id = str_left(rq, ix);
  CgiSession *sdata = opt_nget(cgi_get_session(session_id));

  if (!sdata) {
    cgi_set_key("nosession");
    puts(cgi_expired());

    return 0;
  }

  char *key = cgiSession_key(sdata);
  char *connection_id = cgiSession_id(sdata);
  cgi_set_key(key);
  // Map[Json]
  Map *m = js_ro((Js *)cryp_decryp(str_right(rq, ix + 1), key));
  // Opt[Js]
  Opt *conn_id_js = map_get(m, "connectionId");
  if (opt_is_full(conn_id_js)) {
    char *conn_id = js_rs(opt_get(conn_id_js));
    if (!str_eq(conn_id, connection_id)) {
      cgi_set_key("nosession");
      puts(cgi_expired());

      return 0;
    }
  }

  puts(process(session_id, m));

  return 0;
}
