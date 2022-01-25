// Copyright 13-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "pgs/settings.h"
#include "dmc/cgi.h"
#include "dmc/str.h"
#include "dmc/js.h"
#include "data/cts.h"
#include "db/conftb.h"
#include <stdio.h>

char *settings_process (Mchar *mrq) {
  char *rq = cgi_rq_string(mrq, "rq");
  if (str_eq(rq, "getLang")) {
    Mchar *rp = mchar_new();
    /**/void fn(void) { mchar_put(rp, "lang", js_ws(conftb_read()->lang)); }
    async_run(fn);
    return cgi_rp(rp);
  } else if (str_eq(rq, "setLang")) {
    char *lang = cgi_rq_string(mrq, "lang");
    /**/void fn(void) { conftb_write(conf_new(lang)); }
    async_run(fn);
    return cgi_rp_empty();
  } else return SFAIL(str_f("Unexpected value for 'rq': %s", rq));
}
