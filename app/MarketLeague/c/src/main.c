// Copyright 26-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "main.h"
#include "dmc/cgi.h"
#include "dmc/cryp.h"
#include "pages/pgmain.h"
#include "pages/settings.h"
#include "pages/backups.h"
#include "io/backupsdb.h"
#include "pages/leagues.h"
#include "pages/league.h"

static char *app_name = "MarketLeague";
 static char *data_version = "201908";
static char *app_dir = "dmcgi/MarketLeague";
static time_t expiration = 3600;

// mrq is Map[Js]
static char *process(char *session_id, Map *mrq) {
  CGI_GET_STR(page, mrq)

  if (str_eq(page, "logout")) {
    backupsdb_mkautobackup();
    return cgi_del_session(session_id);
  }
  if (str_eq(page, "Main")) return pgmain_process(mrq);
  if (str_eq(page, "Settings")) return settings_process(mrq);
  if (str_eq(page, "Backups")) return backups_process(mrq);
  if (str_eq(page, "Leagues")) return leagues_process(mrq);
  if (str_eq(page, "League")) return league_process(mrq);

  EXC_ILLEGAL_ARGUMENT(
    "page", "logout | Main | Settings | Backups | Leagues | League", page
  )
  return NULL; // Unreachable
}

int main (int argc, char *argv[]) {
  exc_init();

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

char *main_app_name (void) {
  return app_name;
}

char *main_data_version (void) {
  return data_version;
}
