// Copyright 11-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "pgs/modulePg.h"
#include "dmc/cgi.h"
#include "dmc/str.h"
#include "dmc/Js.h"
#include "dmc/DEFS.h"
#include "dmc/err.h"
#include "data/Doc/ODoc.h"
#include "readers/moduleRd.h"

char *modulePg_process(Mchar *mrq) {
  char *rq = cgi_rq_string(mrq, "rq");
  if (str_eq(rq, "doc")) {
    char *module = cgi_rq_string(mrq, "module");
    char *path = cgi_rq_string(mrq, "path");
    Doc *doc = oDoc_nsome(moduleRd_read(module, path));
    Mchar *rp = mchar_new();
    mchar_put(rp, "doc", doc ? doc_to_js(doc) : js_wn());
    return cgi_rp(rp);
  } else {
    return FAIL(str_f("Unexpected value for 'rq': %s", rq));
  }
}
