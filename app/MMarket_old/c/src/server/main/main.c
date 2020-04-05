// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/main/main.h"
#include "dmc/cgi.h"
#include "db/global.h"

char *mainMain_process (Map *mrq) {
  CGI_GET_STR(rq, mrq);
  if (str_eq(rq, "lang")) {
    Map *rp = map_new();
    map_put(rp, "lang", js_ws(global_lang()));
    return cgi_ok(rp);
  }
  if (str_eq(rq, "bye")) {
    CGI_GET_STR(sessionId, mrq);
    return cgi_del_session(sessionId);
  }

  EXC_ILLEGAL_ARGUMENT("rq", "lang | bye", rq)
  return NULL; // Unreachable
}
