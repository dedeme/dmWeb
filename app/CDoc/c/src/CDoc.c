// Copyright 05-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include <stdio.h>
#include "dmc/cgi.h"
#include "dmc/char/Mchar.h"
#include "dmc/str.h"
#include "dmc/js.h"
#include "dmc/err.h"
#include "dmc/cryp.h"
#include "CDoc.h"
#include "cts.h"
#include "data/Dpath/ADpath.h"
#include "db.h"
#include "db/conftb.h"
#include "db/dpaths.h"
#include "pgs/changePass.h"
#include "pgs/pathsPg.h"
#include "pgs/indexPg.h"
#include "pgs/modulePg.h"
#include "pgs/codePg.h"

static char *cDoc_main_process (Mchar *mrq) {
  char *rq = cgi_rq_string(mrq, "rq");
  if (str_eq(rq, "idata")) {
    Mchar *rp = mchar_new();
    mchar_put(rp, "conf", conftb_read_js());
    mchar_put(rp, "paths", js_wa((Achar *)aDpath_map(
      dpaths_read(),
      (void *(*)(Dpath *))dpath_to_js
    )));
    return cgi_rp(rp);
  } else if (str_eq(rq, "close")) {
    char *sessionId = cgi_rq_string(mrq, "sessionId");
    cgi_remove_session(sessionId);
    return cgi_rp_empty();
  } else if (str_eq(rq, "savePath")) {
    char *path = cgi_rq_string(mrq, "path");
    Conf *cf = conftb_read();
    conftb_write(conf_new(
      path,
      cf->lang,
      cf->show_all
    ));
    return cgi_rp_empty();
  } else {
    return FAIL(str_f("Unexpected value for 'rq': %s", rq));
  }
}

static char *cDoc_process (Mchar *mrq) {
  char *source = cgi_rq_string(mrq, "source");
  if (str_eq(source, "Main")) return cDoc_main_process(mrq);
  else if (str_eq(source, "ChangePass")) return changePass_process(mrq);
  else if (str_eq(source, "PathsPg")) return pathsPg_process(mrq);
  else if (str_eq(source, "IndexPg")) return indexPg_process(mrq);
  else if (str_eq(source, "ModulePg")) return modulePg_process(mrq);
  else if (str_eq(source, "CodePg")) return codePg_process(mrq);
  else return FAIL(str_f("Unexpected value for 'source': %s", source));
}

int main(int argc, char *argv[]) {
  if (argc != 2)  FAIL("CDoc need one and only one argument.");

  char *rq = argv[1];

  cgi_init(cts_app_dir(), cts_expiration());
  db_init(cgi_home());

  int ix = str_cindex(rq, ':');

  //............................................................. CONNECTION
  if (ix == -1) {
    puts(cgi_connect(rq));
    return 0;
  }

  //......................................................... AUTHENTICATION
  if (ix == 0) {
    char *key = cryp_key(cts_app_name(), cgi_klen());
    char *data = cryp_decryp(key, rq + 1);
    Achar *parts = str_csplit(data, ':');
    puts(cgi_authentication(
      key,
      achar_get(parts, 0),
      achar_get(parts, 1),
      *(achar_get(parts, 2)) == '1'
    ));
    return 0;
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
    puts(cDoc_process(js_ro(cryp_decryp(key, data))));
    return 0;
  }

  puts(cgi_rp_expired());
  return 0;
}
