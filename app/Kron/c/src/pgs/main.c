// Copyright 13-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "pgs/main.h"
#include "dmc/cgi.h"
#include "dmc/str.h"
#include "data/cts.h"
#include "db/conftb.h"
#include <stdio.h>

char *main_process (Mchar *mrq) {
  char *rq = cgi_rq_string(mrq, "rq");
  if (str_eq(rq, "idata")) {
    Mchar *rp = mchar_new();
    /**/void fn(void) { mchar_put(rp, "conf", conftb_read_js()); }
    async_run(fn);
    return cgi_rp(rp);
  } else if (str_eq(rq, "close")) {
    char *sessionId = cgi_rq_string(mrq, "sessionId");
    cgi_remove_session(sessionId);
    return cgi_rp_empty();
  } else return SFAIL(str_f("Unexpected value for 'rq': %s", rq));
}
