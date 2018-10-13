// Copyright 04-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "main.h"
#include "conf.h"
#include "dmc/Date.h"
#include "dmc/cgi.h"
#include "dmc/ext.h"
#include "dmc/b64.h"
#include "dmc/cryp.h"
#include "dmc/ct/Ajson.h"
#include "dmc/ct/Mjson.h"
#include "core/backups.h"
#include "core/chpass.h"
#include "core/settings.h"

static char *app_name = "FleasData";
static char *data_version = "201810";
static char *app_dir = "dmcgi/FleasData";
static time_t expiration = 3600;
static char *fleas_dir = "/home/deme/.dmCrystal/fleas";

static void app_init(void) {
	char *dir = path_cat(cgi_home(), "data", NULL);

  if (!file_exists(dir)) {
    file_mkdir(dir);
    char *version = str_printf(
        "%s\nData version: %s\n", app_name, data_version);
    char *fversion = path_cat(dir, "version.txt", NULL);
    file_write(fversion, version);
    file_mkdir(path_cat(cgi_home(), "tmp", NULL));
    file_mkdir(path_cat(cgi_home(), "trash", NULL));
    file_mkdir(path_cat(cgi_home(), "backups", NULL));
  }
}

static Achar *get_families(void) {
  Achar *r = achar_new();
  EACH(file_dir(fleas_dir), char, f) {
    if (file_is_directory(f)) {
      achar_add(r, path_name(f));
    }
  }_EACH
  return r;
}

// ____________
// Main process
// TTTTTTTTTTTT

static CgiRp *main_process(char *session_id, Mjson *rqm) {
  char *rq = jmap_gstring(rqm, "rq");

  // ---------------------------------------------------------- logout
  if (str_eq(rq, "logout")) {
    backups_process(app_name, data_version, rqm);
    return cgi_del_session(session_id);
  }

  // ----------------------------------------------------------- getDb
  if (str_eq(rq, "getDb")) {
    Achar *families = get_families();
    Mjson *m = mjson_new();
    mjson_put(m, "conf", conf_get());
    mjson_put(m, "families", achar_to_json(families));
    return cgi_ok(m);
  }

  // --------------------------------------------------------- setMenu
  if (str_eq(rq, "setMenu")) {
    char *page = jmap_gstring(rqm, "targetPage");
    char *family = jmap_gstring(rqm, "family");
    conf_set_menu(page, family);
    return cgi_ok(mjson_new());
  }

  // --------------------------------------------------------- setMenu
  if (str_eq(rq, "getData")) {
    char *family = jmap_gstring(rqm, "family");
    char part = *jmap_gstring(rqm, "part");

    bool ok = false;
    char *path = "";
    Mjson *data = mjson_new();
    path = path_cat(fleas_dir, family, "historic", NULL);
    if (file_is_directory(path)) {
      EACH(file_dir(path), char, f) {
        char *cycle = str_sub_end(path_name(f), 1);
        if (*cycle == part) {
          mjson_put(data, cycle, (Json *)file_read(f));
        }
      }_EACH
    }

    Mjson *m = mjson_new();
    mjson_put(m, "data", json_wobject(data));
    return cgi_ok(m);
  }

  // ---------------------------------------------------------- error!
  THROW("") "'%s': Unknown main request", rq _THROW
  // Unreacheable
  return NULL;
}

// ______________
// Commun process
// TTTTTTTTTTTTTT

static CgiRp *app_process(char *session_id, Mjson *rqm) {
  char *page = jmap_gstring(rqm, "page");

  // ------------------------------------------------------- Main page
  if (str_eq(page, "main")) {
    return main_process(session_id, rqm);
  }

  // ---------------------------------------------------- Backups page
  if (str_eq(page, "backups")) {
    return backups_process(app_name, data_version, rqm);
  }

  // ----------------------------------------------------- Chapss page
  if (str_eq(page, "chpass")) {
    return chpass_process(rqm);
  }

  // --------------------------------------------------- Settings page
  if (str_eq(page, "settings")) {
    return settings_process(rqm);
  }

  // ---------------------------------------------------------- error!
  THROW("") "'%s': Unknown page request", page _THROW
  // Unreacheable
  return NULL;
}


static void send (CgiRp *rp) {
  puts ((char *) rp);
}

int main (int argc, char **argv) {
  exc_init();
  TRY {
    if (argc != 2) {
      THROW("") "argc must be 2" _THROW
    }

    cgi_init(app_dir, expiration);
    app_init();
    conf_init();

    char *rq = argv[1];
    int ix = str_cindex(rq, ':');
    //............................................................. CONNECTION
    if (ix == -1) {
      cgi_set_key(rq);
      send(cgi_connect(rq));
      return;
    }
    //......................................................... AUTHENTICATION
    if (ix == 0) {
      char *key = cryp_key(app_name, cgi_klen());
      cgi_set_key(key);

      char *data = cryp_decryp(key, rq + 1);
      Achar *parts = str_csplit(data, ':');
      send(cgi_authentication(
        achar_get(parts, 0),
        achar_get(parts, 1),
        *achar_get(parts, 2) == '1'
      ));
      return; }
    //............................................................ NORMAL DATA
    char *session_id = str_sub(rq, 0, ix);
    char *key;
    char *connectionId;
    cgi_get_session_data(&key, &connectionId, session_id);

    if (!*key) {
      cgi_set_key("nosession");
      send(cgi_expired());
      return;
    }

    cgi_set_key(key);
    Mjson *m = json_robject((Json *)cryp_decryp(key, str_sub_end(rq, ix + 1)));
    if (
      mjson_has_key(m, "connectionId") &&
      !str_eq(connectionId, jmap_gstring(m, "connectionId"))
    ) {
      cgi_set_key("nosession");
      send(cgi_expired());
      return;
    }

    send(app_process(session_id, m));
  } CATCH (e) {
    puts (e);
  }_TRY

  return 0;
}
