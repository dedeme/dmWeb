// Copyright 15-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "pgs/main.h"
#include "dmc/cgi.h"
#include "dmc/str.h"
#include "dmc/err.h"
#include "dmc/js.h"
#include "data/models.h"
#include "data/Model/AModel.h"

char *main_process (Mchar *mrq) {
  char *rq = cgi_rq_string(mrq, "rq");
  if (str_eq(rq, "idata")) {
    Mchar *rp = mchar_new();
    Achar *ls = models_ids_list();
    mchar_put(rp, "mainModelId", js_ws(*ls->es));
    mchar_put(rp, "modelIds", achar_to_js(ls));
    return cgi_rp(rp);
  } else if (str_eq(rq, "close")) {
    char *sessionId = cgi_rq_string(mrq, "sessionId");
    cgi_remove_session(sessionId);
    return cgi_rp_empty();
  } else return FAIL(str_f("Unexpected value for 'rq': %s", rq));
}
