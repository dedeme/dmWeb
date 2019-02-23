// Copyright 05-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "core/chpass.h"
#include "dmc/cgi.h"

// 'rq' is Map[Js]
void chpass_process(Map *rq) {
  CGI_GET_STR(user, rq, "user");
  CGI_GET_STR(pass, rq, "pass");
  CGI_GET_STR(new_pass, rq, "newPass");
  cgi_change_pass(user, pass, new_pass);
  free(user);
  free(pass);
  free(new_pass);
}
