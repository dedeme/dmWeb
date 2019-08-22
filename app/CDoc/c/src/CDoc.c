// Copyright 18-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "CDoc.h"
#include "dmc/cgi.h"
#include "dmc/cryp.h"
#include "pages/main.h"
#include "pages/paths.h"
#include "pages/chpass.h"
#include "pages/index.h"
#include "pages/module.h"
#include "pages/code.h"

static char *app_name = "CDoc";
static char *app_dir = "dmcgi/CDoc";
static time_t expiration = 3600;

// mrq is Map[Js]
static char *process (Map *mrq) {
  CGI_GET_STR(page, mrq)

  if (str_eq(page, "Main")) return main_process(mrq);
  if (str_eq(page, "Paths")) return paths_process(mrq);
  if (str_eq(page, "Chpass")) return chpass_process(mrq);
  if (str_eq(page, "Index")) return index_process(mrq);
  if (str_eq(page, "Module")) return module_process(mrq);
  if (str_eq(page, "Code")) return code_process(mrq);

  EXC_ILLEGAL_ARGUMENT(
    "page", "Main | Paths | Chpass | Index | Module | Code", page
  )
  return NULL;  // Unreachable
}

int main (int argc, char *argv[]) {
  cgi_init(app_dir, expiration);
  exc_init();

  if (argc != 2)
    EXC_ILLEGAL_ARGUMENT("argc", "2", str_f("%d", argc))

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
  // Opt [CgiSession]
  Opt *ossdata = cgi_get_session(session_id);

  if (opt_is_empty(ossdata)) {
    cgi_set_key("nosession");
    puts(cgi_expired());
    return 0;
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
      puts(cgi_expired());
      return 0;
    }
  }

  //..................................... logout
  CGI_GET_STR(page, m);

  if (str_eq(page, "logout")) {
    puts(cgi_del_session(session_id));
    return 0;
  }

  puts(process(m));
  return 0;
}
