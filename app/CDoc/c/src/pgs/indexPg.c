// Copyright 10-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "pgs/indexPg.h"
#include "dmc/cgi.h"
#include "dmc/str.h"
#include "dmc/Js.h"
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "data/IndexTree/OIndexTree.h"
#include "readers/indexRd.h"

char *indexPg_process(Mchar *mrq) {
  char *rq = cgi_rq_string(mrq, "rq");
  if (str_eq(rq, "index")) {
    char *module = cgi_rq_string(mrq, "module");
    IndexTree *ixt = oIndexTree_nsome(indexRd_read(module));
    Mchar *rp = mchar_new();
    mchar_put(rp, "index", ixt ? indexTree_to_js(ixt) : js_wn());
    return cgi_rp(rp);
  } else {
    return FAIL(str_f("Unexpected value for 'rq': %s", rq));
  }
}
