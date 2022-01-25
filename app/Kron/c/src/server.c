// Copyright 13-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server.h"
#include <stdlib.h>
#include "dmc/Iserver.h"
#include "dmc/err.h"
#include "dmc/str.h"
#include "dmc/sys.h"
#include "dmc/cgi.h"
#include "dmc/cryp.h"
#include "dmc/js.h"
#include "data/cts.h"
#include "pgs/main.h"
#include "pgs/home.h"
#include "pgs/fix.h"
#include "pgs/periodic.h"
#include "pgs/init.h"
#include "pgs/notes.h"
#include "pgs/settings.h"
#include "pgs/changePass.h"
#include "scheduler.h"

static char *server_source_process (Mchar *mrq) {
  char *source = cgi_rq_string(mrq, "source");
  if (str_eq(source, "Main")) return main_process(mrq);
  else if (str_eq(source, "Home")) return home_process(mrq);
  else if (str_eq(source, "Periodic")) return periodic_process(mrq);
  else if (str_eq(source, "Fix")) return fix_process(mrq);
  else if (str_eq(source, "Init")) return init_process(mrq);
  else if (str_eq(source, "Notes")) return notes_process(mrq);
  else if (str_eq(source, "Settings")) return settings_process(mrq);
  else if (str_eq(source, "ChangePass")) return changePass_process(mrq);
  else return SFAIL(str_f("Unexpected value for 'source': %s", source));
}

static char *server_process(char *rq) {
  int ix = str_cindex(rq, ':');

  //............................................................. CONNECTION
  if (ix == -1) {
    return(cgi_connect(rq));
  }

  //......................................................... AUTHENTICATION
  if (ix == 0) {
    char *key = cryp_key(cts_app_name(), cgi_klen());
    char *data = cryp_decryp(key, rq + 1);
    Achar *parts = str_csplit(data, ':');
    return(cgi_authentication(
      key,
      achar_get(parts, 0),
      achar_get(parts, 1),
      *(achar_get(parts, 2)) == '1'
    ));
  }

  //............................................................ NORMAL DATA
  Achar *parts = str_csplit(rq, ':');
  char *session_id = achar_get(parts, 0);
  char *data = achar_get(parts, 1);
  char *con_key = "";
  if (achar_size(parts) > 2) {
    con_key = data;
    data = achar_get(parts, 2);
  }

  char *key = ochar_nsome(cgi_set_key(session_id, con_key));
  if (key) {
    return(server_source_process(js_ro(cryp_decryp(key, data))));
  }

  return(cgi_rp_expired());
}

void server_run (void) {
  Iserver *is = iserver_new(atoi(cts_port()));
  for (;;) {
    IserverRq *rq = iserver_read(is);
    if (*iserverRq_error(rq))
      FAIL(iserverRq_error(rq));
    char *request = ochar_nsome(iserverRq_msg(rq));
    if (request) {
      if (str_eq(request, cts_server_stop_msg())) {
        iserver_close(is);
        scheduler_stop();
        return;
      }
      if (str_eq(request, "test")) {
        iserverRq_write(rq, cts_server_ok_msg());
      } else {
        /**/void fn (void *rq) {
        /**/  iserverRq_write(
        /**/    rq, server_process(ochar_some(iserverRq_msg(rq)))
        /**/  );
        /**/}
        async_thread_detached(fn, rq);
      }
    }
    sys_sleep(200);
  }
}
