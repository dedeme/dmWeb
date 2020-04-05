// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/settings/settings.h"
#include "dmc/cgi.h"
#include "db/global.h"

char *settings_process (AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(rq, mrq);
  if (str_eq(rq, "setLang")) {
    CGI_GET_STR(lang, mrq);
    void fn () { global_set_lang(lang); }
    asyncActor_wait(ac, fn);
    return cgi_empty();
  };

  EXC_ILLEGAL_ARGUMENT("rq", "setLang", rq)
  return NULL; // Unreachable
}
