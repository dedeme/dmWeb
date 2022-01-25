// Copyright 30-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "web/docPg.h"
#include "dmc/cgi.h"
#include "dmc/str.h"
#include "dmc/err.h"
#include "data/cts.h"
#include "strategies.h"

char *docPg_process (Mchar *mrq) {
  char *rq = cgi_rq_string(mrq, "rq");
  if (str_eq(rq, "idata")) {
    Mchar *doc = mchar_new();
    AStrategy *ss = strategies_list();
    Strategy **p = ss->es;
    while (p < ss->end) {
      Strategy *s = *p++;
      mchar_put(doc, s->id(), s->doc());
    }
    Mchar *rp = mchar_new();
    mchar_put(rp, "doc", mchar_to_js(doc));
    return cgi_rp(rp);
  } else return FAIL(str_f("Unexpected value for 'rq': %s", rq));
}
