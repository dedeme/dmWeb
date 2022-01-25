// Copyright 21-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "pgs/notes.h"
#include "dmc/cgi.h"
#include "dmc/str.h"
#include "dmc/js.h"
#include "data/cts.h"
#include "db/anns.h"
#include "db/log.h"
#include <stdio.h>

char *notes_process (Mchar *mrq) {
  char *rq = cgi_rq_string(mrq, "rq");
  if (str_eq(rq, "idata")) {
    Mchar *rp = mchar_new();
    AAnn* anns = NULL;
    /**/void fn(void) { anns = anns_read(); }
    async_run(fn);
    /**/int filter(Ann *ann) { return ann->time_type == ann_NOTE; }
    aAnn_filter_in(anns, filter);
    mchar_put(rp, "anns", aAnn_to_js(anns));
    return cgi_rp(rp);
  } else if (str_eq(rq, "new")) {
    Ann *ann = ann_from_js(ochar_some(mchar_get(mrq, "ann")));
    /**/void fn(void) {
      AAnn *anns = anns_read();
      aAnn_add(anns, ann);
      anns_write(anns);
    }
    async_run(fn);
    return cgi_rp_empty();
  } else if (str_eq(rq, "modify")) {
    Ann *ann = ann_from_js(ochar_some(mchar_get(mrq, "ann")));
    /**/void fn(void) {
      AAnn *anns = anns_read();
      aAnn_modify(anns, ann);
      anns_write(anns);
    }
    async_run(fn);
    return cgi_rp_empty();
  } else if (str_eq(rq, "delete")) {
    int id = cgi_rq_int(mrq, "id");
    /**/void fn(void) {
      AAnn *anns = anns_read();
      aAnn_delete(anns, id);
      anns_write(anns);
    }
    async_run(fn);
    return cgi_rp_empty();
  } else return SFAIL(str_f("Unexpected value for 'rq': %s", rq));
}
