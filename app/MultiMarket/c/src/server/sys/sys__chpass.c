// Copyright 05-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/sys/sys__chpass.h"
#include "dmc/cgi.h"

// mrq is Map[Js]
char *sys__chpass_process(AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(user, mrq, "user")
  CGI_GET_STR(pass, mrq, "pass")
  CGI_GET_STR(new_pass, mrq, "newPass")

  char *r = NULL;
  void fn () { r = cgi_change_pass(user, pass, new_pass); }
  asyncActor_wait(ac, fn);
  return r;
}
