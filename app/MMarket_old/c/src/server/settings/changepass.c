// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/settings/changepass.h"
#include "dmc/cgi.h"

char *changepass_process (AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(user, mrq);
  CGI_GET_STR(old, mrq);
  CGI_GET_STR(new, mrq);

  char *rp;
  void fn () { rp = cgi_change_pass(user, old, new); }
  asyncActor_wait(ac, fn);
  return rp;
}

