// Copyright 04-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "main.h"
#include "db.h"
#include "dmc/Date.h"
#include "dmc/cgi.h"
#include "dmc/ext.h"
#include "dmc/b64.h"
#include "dmc/cryp.h"
#include "dmc/ct/Ajson.h"
#include "dmc/ct/Mjson.h"
#include "core/chpass.h"
#include "paths.h"
#include "index.h"
#include "module.h"
#include "code.h"

static char *app_name = "JsDoc";
static char *data_version = "201809";
static char *app_dir = "dmcgi/JsDoc";
static time_t expiration = 3600;

static void app_init(void) {
	char *dir = path_cat(cgi_home(), "data", NULL);

  if (!file_exists(dir)) {
    file_mkdir(dir);
    char *version = str_printf(
        "%s\nData version: %s\n", app_name, data_version);
    char *fversion = path_cat(dir, "version.txt", NULL);
    file_write(fversion, version);
  }
}

// ____________
// Main process
// TTTTTTTTTTTT

static CgiRp *main_process(char *session_id, Mjson *rqm) {
  char *rq = jmap_gstring(rqm, "rq");

  // ---------------------------------------------------------- logout
  if (str_eq(rq, "logout")) {
    return cgi_del_session(session_id);
  }

  // ----------------------------------------------------------- idata
  if (str_eq(rq, "idata")) {
    Mjson *m = db_conf();
    mjson_put(m, "paths", db_paths());
    return cgi_ok(m);
  }

  // -------------------------------------------------------------- go
  if (str_eq(rq, "go")) {
    char *path = jmap_gstring(rqm, "path");
    db_set_path(path);
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

  // ----------------------------------------------------- Chapss page
  if (str_eq(page, "chpass")) {
    return chpass_process(rqm);
  }

  // ------------------------------------------------------ Paths page
  if (str_eq(page, "paths")) {
    return paths_process(rqm);
  }

  // ------------------------------------------------------ Index page
  if (str_eq(page, "index")) {
    return index_process(rqm);
  }

  // ----------------------------------------------------- Module page
  if (str_eq(page, "module")) {
    return module_process(rqm);
  }

  // ------------------------------------------------------- Code page
  if (str_eq(page, "code")) {
    return code_process(rqm);
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
    db_init();

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
    EACH(exc_stack(e), char, l) {
      puts(l);
    }_EACH
  }_TRY

  return 0;
}
