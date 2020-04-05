// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/fleas/tests.h"
#include "dmc/cgi.h"
#include "data/flea/fmodels.h"
#include "data/flea/Fmodel.h"
#include "db/fleas/flog.h"

char *testsFleas_process (AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(rq, mrq);

  if (str_eq(rq, "idata")) {
    Map *rp = map_new();
    map_put(rp, "list", arr_to_js(fmodels_list(), (FTO)fmodel_to_js));
    map_put(rp, "logId", js_ws(flog_new()));
    return cgi_ok(rp);
  }

  EXC_ILLEGAL_ARGUMENT("rq", "idata", rq)
  return NULL; // Unreachable
}

