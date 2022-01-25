// Copyright 13-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "pgs/home.h"
#include "dmc/cgi.h"
#include "dmc/str.h"
#include "data/cts.h"
#include "db/log.h"
#include <stdio.h>

char *home_process (Mchar *mrq) {
  char *rq = cgi_rq_string(mrq, "rq");
  if (str_eq(rq, "log")) {
    Mchar *rp = mchar_new();
    /**/void fn(void) { mchar_put(rp, "log", log_read_js()); }
    async_run(fn);
    return cgi_rp(rp);
  } else if (str_eq(rq, "reset")) {
    /**/void fn(void) { log_write(aLogEntry_new()); }
    async_run(fn);
    return cgi_rp_empty();
  } else return SFAIL(str_f("Unexpected value for 'rq': %s", rq));
}
