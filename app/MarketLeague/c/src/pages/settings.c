// Copyright 26-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "pages/settings.h"
#include "dmc/cgi.h"
#include "io/conf.h"

char *settings_process(Map *mrq) {
  CGI_GET_STR(rq, mrq)

  if (str_eq(rq, "setLang")) {
    CGI_GET_STR(lang, mrq);

    conf_set_lang(lang);
    return cgi_empty();
  }
  if (str_eq(rq, "chpass")) {
    CGI_GET_STR(user, mrq);
    CGI_GET_STR(pass, mrq);
    CGI_GET_STR(newPass, mrq);

    return cgi_change_pass(user, pass, newPass);
  }

  EXC_ILLEGAL_ARGUMENT("rq", "setLang", rq)
  return NULL; // Unreachable
}

