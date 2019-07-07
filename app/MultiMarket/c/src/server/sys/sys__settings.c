// Copyright 05-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/sys/sys__settings.h"
#include "dmc/cgi.h"
#include "io/conf.h"

// mrq is Map[Js]
char *sys__settings_process(AsyncActor *ac, Map *mrq) {
  // Map[Js]
  Map *rp = map_new();
  CGI_GET_STR(rq, mrq, "rq")

  if (str_eq(rq, "getLang")) {
    void fn (void *null) { map_put(rp, "lang", js_ws(conf_lang())); }
    asyncActor_wait(ac, fn, NULL);
    return cgi_ok(rp);
  }
  if (str_eq(rq, "setLang")) {
    CGI_GET_STR(lang, mrq, "lang")
    asyncActor_wait(ac, (FPROC)conf_set_lang, lang);
    return cgi_empty();
  }

  EXC_ILLEGAL_ARGUMENT("rq", "getLang", rq)
  return NULL; // Unreachable
}
