// Copyright 04-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "main.h"
#include "conf.h"
#include "dmc/date.h"
#include "dmc/cgi.h"
#include "dmc/ext.h"
#include "dmc/b64.h"
#include "dmc/cryp.h"
#include "core/backups.h"
#include "core/chpass.h"
#include "core/settings.h"
#include "DEFS.h"
#include "data.h"

static char *app_name = "FleasData";
static char *data_version = "201810";
static char *app_dir = "dmcgi/FleasData";
static time_t expiration = 3600;

static void app_init(void) {
  void mkdir(const char *name) {
    char *f = path_cat_new(cgi_home(), name, NULL);
    file_mkdir(f);
    free(f);
  }

	char *dir = path_cat_new(cgi_home(), "data", NULL);

  if (!file_exists(dir)) {
    file_mkdir(dir);
    char *version = str_f_new(
        "%s\nData version: %s\n", app_name, data_version);
    char *fversion = path_cat_new(dir, "version.txt", NULL);
    file_write(fversion, version);
    free(version);
    free(fversion);

    mkdir("tmp");
    mkdir("trash");
    mkdir("backups");
  }

  free(dir);
}

// Returns a sorted Arr[Js] serialized to Js
static Js *fgroups_new(void) {
  // Arr[char]
  Arr *ajs = arr_new(free);
  // Arr[char]
  Arr *fs = file_dir_new(FLEAS_DIR);
  arr_sort(fs, (FGREATER)str_greater);
  EACH(fs, char, f)
    char *path = path_cat_new(FLEAS_DIR, f, NULL);
    if (file_is_directory(path)) {
      arr_push(ajs, js_ws_new(f));
    }
    free(path);
  _EACH
  arr_free(fs);
  Js *r = js_wa_new(ajs);
  arr_free(ajs);
  return r;
}

// Returns constants map
static Js *constants_new(void) {
  char *f = path_cat_new(FLEAS_DIR, "conf.db", NULL);
  Js *r = (Js *)file_read_new(f);
  free(f);
  return r;
}

// ____________
// Main process
// TTTTTTTTTTTT

// 'rqm' is Map[Js]
static void main_process(const char *session_id, Map *rqm) {
  CGI_GET_STR(rq, rqm, "rq")

  // ---------------------------------------------------------- logout
  if (str_eq(rq, "logout")) {
    backups_process(app_name, data_version, rqm);
    cgi_del_session(session_id);

  // ----------------------------------------------------------- getDb
  } else if (str_eq(rq, "getDb")) {
    // Map[Js]
    Map *m = map_new(free);
    map_put(m, "db", conf_get_new());
    map_put(m, "fgroups", fgroups_new());
    map_put(m, "constants", constants_new());
    cgi_ok(m);
    map_free(m);

  // --------------------------------------------------------- setMenu
  } else if (str_eq(rq, "setMenu")) {
    CGI_GET_STR(tmenu, rqm, "tmenu")
    CGI_GET_STR(lmenu, rqm, "lmenu")
    conf_set_tmenu(tmenu);
    conf_set_lmenu(lmenu);
    cgi_empty();
    free(tmenu);
    free(lmenu);

  // ---------------------------------------------------------- error!
  } else FAIL(str_f_new("Unknown request '%s'", rq))

  free(rq);
}

// ______________
// Commun process
// TTTTTTTTTTTTTT

// 'rqm' is Map[Js]
static void app_process(const char *session_id, Map *rqm) {
  CGI_GET_STR(source, rqm, "source")

  // ------------------------------------------------------- Main page
  if (str_eq(source, "main")) {
    main_process(session_id, rqm);

  // ---------------------------------------------------- Backups page
  } else if (str_eq(source, "backups")) {
    backups_process(app_name, data_version, rqm);

  // ----------------------------------------------------- Chapss page
  } else if (str_eq(source, "chpass")) {
    chpass_process(rqm);

  // --------------------------------------------------- Settings page
  } else if (str_eq(source, "settings")) {
    settings_process(rqm);

  // ------------------------------------------------------- Data page
  } else if (str_eq(source, "Data")) {
    data_process(rqm);

  // ---------------------------------------------------------- error!
  } else FAIL(str_f_new("Unknown source '%s'", source))

  free(source);
}


int main (int argc, char **argv) {
  if (argc != 2) FAIL("argc must be 2")

  cgi_init(app_dir, expiration);
  app_init();
  conf_init();

  char *rq = argv[1];
  int ix = str_cindex(rq, ':');
  //............................................................. CONNECTION
  if (ix == -1) {
    cgi_set_key(rq);
    cgi_connect(rq);

  //......................................................... AUTHENTICATION
  } else if (ix == 0) {
    char *key = str_new(app_name);
    cryp_key(&key, cgi_klen());
    cgi_set_key(key);

    char *data = str_new(rq + 1);
    cryp_decryp(&data, key);
    free(key);
    // Arr[char]
    Arr *parts = str_csplit_new(data, ':');
    free(data);
    cgi_authentication(
      arr_get(parts, 0),
      arr_get(parts, 1),
      *((char *)arr_get(parts, 2)) == '1'
    );
    arr_free(parts);

  //............................................................ NORMAL DATA
  } else {
    char *session_id = str_new(rq);
    str_left(&session_id, ix);
    char *key;
    char *connection_id;
    cgi_get_session_data(&key, &connection_id, session_id);

    void send_expired(void) {
      cgi_set_key("nosession");
      cgi_expired();
      free(key);
      free(connection_id);
      free(session_id);
    }

    if (!*key) {
      send_expired();
      cgi_end();
      return 0;
    }

    cgi_set_key(key);
    char *data = str_new(rq);
    str_right(&data, ix + 1);
    cryp_decryp(&data, key);

    // Map[Js]
    Map *m = js_ro_new((Js *)data);
    free(data);
    Js *conn_id_js = map_get_null(m, "connectionId");
    if (conn_id_js) {
      char *conn_id = js_rs_new(conn_id_js);
      if (!str_eq(conn_id, connection_id)) {
        map_free(m);
        free(conn_id);
        send_expired();
        cgi_end();
        return 0;
      }
      free(conn_id);
    }

    free(key);
    free(connection_id);

    app_process(session_id, m);

    map_free(m);
    free(session_id);
  }

  cgi_end();

  return 0;
}
