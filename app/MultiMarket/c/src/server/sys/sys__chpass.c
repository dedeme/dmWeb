// Copyright 05-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/sys/sys__chpass.h"
#include "dmc/cgi.h"

// mrq is Map[Js]
char *sys__chpass_process(Map *mrq) {
  CGI_GET_STR(user, mrq, "user")
  CGI_GET_STR(pass, mrq, "pass")
  CGI_GET_STR(new_pass, mrq, "newPass")

  return cgi_change_pass(user, pass, new_pass);
}
