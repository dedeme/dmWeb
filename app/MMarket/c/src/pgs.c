// Copyright 13-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "pgs.h"
#include <stdlib.h>
#include "dmc/err.h"
#include "dmc/str.h"
#include "dmc/sys.h"
#include "dmc/cgi.h"
#include "dmc/cryp.h"
#include "dmc/js.h"
#include "data/cts.h"
#include "pgs/main.h"
#include "pgs/descriptionPg.h"
#include "pgs/resultsPg.h"
#include "pgs/hotPg.h"
#include "pgs/cosPg.h"
#include "pgs/historicPg.h"
#include "pgs/operationsPg.h"
#include "pgs/changePass.h"

static char *server_source_process (Mchar *mrq) {
  char *source = cgi_rq_string(mrq, "source");
  if (str_eq(source, "Main")) return main_process(mrq);
  else if (str_eq(source, "Description")) return descriptionPg_process(mrq);
  else if (str_eq(source, "ResultsPg")) return resultsPg_process(mrq);
  else if (str_eq(source, "HotPg")) return hotPg_process(mrq);
  else if (str_eq(source, "CosPg")) return cosPg_process(mrq);
  else if (str_eq(source, "HistoricPg")) return historicPg_process(mrq);
  else if (str_eq(source, "OperationsPg")) return operationsPg_process(mrq);
  else if (str_eq(source, "ChangePass")) return changePass_process(mrq);
  else return FAIL(str_f("Unexpected value for 'source': %s", source));
}

char *pgs_process(char *rq) {
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
    return server_source_process(js_ro(cryp_decryp(key, data)));
  }

  return(cgi_rp_expired());
}
