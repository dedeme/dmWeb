// Copyright 04-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "main.h"
#include "conf.h"
#include "data/db.h"
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
#include "nicks.h"
#include "edit.h"
#include "issues.h"
#include "servers.h"

static char *app_name = "Quotes";
static char *data_version = "201809";
static char *app_dir = "dmcgi/Quotes";
static time_t expiration = 3600;
static int max_quotes = 750;

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
    db_create(dir);
  }
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
    Mjson *m = mjson_new();
    mjson_put(m, "db", conf_get());
    return cgi_ok(m);
  }

  // --------------------------------------------------------- setMenu
  if (str_eq(rq, "setMenu")) {
    char *option = jmap_gstring(rqm, "option");
    conf_set_menu(option);
    return cgi_ok(mjson_new());
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

  // ------------------------------------------------------ Nicks page
  if (str_eq(page, "nicks")) {
    return nicks_process(rqm);
  }

  // ------------------------------------------------------- Edit page
  if (str_eq(page, "edit")) {
    return edit_process(rqm, max_quotes);
  }

  // ----------------------------------------------------- Issues page
  if (str_eq(page, "issues")) {
    return issues_process(rqm);
  }

  // ---------------------------------------------------- Servers page
  if (str_eq(page, "servers")) {
    return servers_process(rqm);
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
    db_init(path_cat(cgi_home(), "data", NULL));

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
