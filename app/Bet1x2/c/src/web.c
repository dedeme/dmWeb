// Copyright 24-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "web.h"
#include <stdlib.h>
#include "dmc/Iserver.h"
#include "dmc/err.h"
#include "dmc/str.h"
#include "dmc/sys.h"
#include "dmc/cgi.h"
#include "dmc/cryp.h"
#include "dmc/js.h"
#include "data/cts.h"
#include "web/main.h"
#include "web/home.h"
#include "web/standings.h"
#include "web/pointsPg.h"
#include "web/betsPg.h"
#include "web/docPg.h"
#include "web/profits/allTotal.h"
#include "web/profits/yearTotal.h"
#include "web/profits/allStrategy.h"
#include "web/profits/yearStrategy.h"
#include "web/changePass.h"

static char *process (Mchar *mrq) {
  char *source = cgi_rq_string(mrq, "source");
  if (str_eq(source, "Main")) return main_process(mrq);
  else if (str_eq(source, "Home")) return home_process(mrq);
  else if (str_eq(source, "Standings")) return standings_process(mrq);
  else if (str_eq(source, "PointsPg")) return pointsPg_process(mrq);
  else if (str_eq(source, "BetsPg")) return betsPg_process(mrq);
  else if (str_eq(source, "DocPg")) return docPg_process(mrq);
  else if (str_eq(source, "AllTotal")) return allTotal_process(mrq);
  else if (str_eq(source, "YearTotal")) return yearTotal_process(mrq);
  else if (str_eq(source, "AllStrategy")) return allStrategy_process(mrq);
  else if (str_eq(source, "YearStrategy")) return yearStrategy_process(mrq);
  else if (str_eq(source, "ChangePass")) return changePass_process(mrq);
  else return FAIL(str_f("Unexpected value for 'source': %s", source));
}

char *web_process(char *rq) {
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
    return(process(js_ro(cryp_decryp(key, data))));
  }

  return(cgi_rp_expired());
}
