// Copyright 05-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "core/settings.h"
#include "conf.h"

CgiRp *settings_process(Mjson *mrq) {
  char *rq = jmap_gstring(mrq, "rq");

  if (str_eq(rq, "setLang")) {
    char *lang = jmap_gstring(mrq, "lang");
    conf_set_lang(lang);
    return cgi_ok(mjson_new());
  }

  THROW("") "'%s': Unknown request in settings", rq _THROW
  // Unreacheable
  return NULL;

}
