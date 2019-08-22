// Copyright 20-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "pages/chpass.h"
#include "dmc/cgi.h"

// mrq is Map[Js]
char *chpass_process (Map *mrq) {
  CGI_GET_STR(user, mrq)
  CGI_GET_STR(pass, mrq)
  CGI_GET_STR(newPass, mrq)

  return cgi_change_pass(user, pass, newPass);
}
