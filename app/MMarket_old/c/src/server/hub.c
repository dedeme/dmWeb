// Copyright 15-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "server/hub.h"
#include "dmc/cgi.h"
#include "dmc/cryp.h"
#include "server/main/main.h"
#include "server/home/main.h"
#include "server/settings/main.h"
#include "server/fleas/main.h"
#include "DEFS.h"

// rq is Map[Js]
static char *module_process(AsyncActor *ac, Map *mrq) {
  CGI_GET_STR(module, mrq);

  if (str_eq(module, "Main")) return mainMain_process(mrq);
  if (str_eq(module, "Home")) return mainHome_process(ac, mrq);
  if (str_eq(module, "Settings")) return mainSettings_process(ac, mrq);
  if (str_eq(module, "Fleas")) return mainFleas_process(ac, mrq);

  EXC_ILLEGAL_ARGUMENT("module", "Main | Home | Settings", module)
  return NULL; // Unreachable
}

char *hub_rp (AsyncActor *ac, char *rq) {
  int ix = str_cindex(rq, ':');
  //............................................................. CONNECTION
  if (ix == -1) {
    cgi_set_key(rq);
    return cgi_connect(rq);
  }

  //......................................................... AUTHENTICATION
  if (ix == 0) {
    char *key = cryp_key(APP_NAME, cgi_klen());
    cgi_set_key(key);
    char *data = cryp_decryp(rq + 1, key);
    // Arr[char]
    Arr *parts = str_csplit(data, ':');
    return cgi_authentication(
      arr_get(parts, 0),
      arr_get(parts, 1),
      *((char *)arr_get(parts, 2)) == '1'
    );
  }

  //............................................................ NORMAL DATA
  char *session_id = str_left(rq, ix);
  // Opt [CgiSession]
  Opt *ossdata = cgi_get_session(session_id);

  if (opt_is_empty(ossdata)) {
    cgi_set_key("nosession");
    return cgi_expired();
  }

  CgiSession *ssdata = opt_get(ossdata);

  char *key = cgiSession_key(ssdata);
  cgi_set_key(key);
  char *data = cryp_decryp(str_right(rq, ix + 1), key);

  return module_process(ac, js_ro((Js *)data));
}
