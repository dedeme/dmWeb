// Copyright 25-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/acc/acc__downloader.h"
#include "dmc/cgi.h"
#include "io/net.h"

// mrq is Map[Js]
char *acc__downloader_process(AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(rq, mrq)

  if (str_eq(rq, "download")) {
    net_update_daily(ac);
    return cgi_empty();
  }

  EXC_ILLEGAL_ARGUMENT("rq", "download", rq)
  return NULL; // Unreachable
}
