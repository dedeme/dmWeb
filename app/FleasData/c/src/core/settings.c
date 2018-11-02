// Copyright 05-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "core/settings.h"
#include "conf.h"
#include "dmc/cgi.h"

// 'mrq' is Map[Js]
void settings_process(Map *mrq) {
  CGI_GET_STR(rq, mrq, "rq")

  if (str_eq(rq, "setLang")) {
    CGI_GET_STR(lang, mrq, "lang")
    conf_set_lang(lang);
    cgi_empty();
    free(lang);
  } else {
    FAIL(str_f_new("'%s': Unknown 'rq' in settings", rq))
  }

  free(rq);
}
