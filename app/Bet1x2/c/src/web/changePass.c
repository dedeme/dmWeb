// Copyright 24-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "web/changePass.h"
#include "dmc/cgi.h"
#include "dmc/str.h"
#include "dmc/err.h"
#include "data/cts.h"
#include <stdio.h>

char *changePass_process (Mchar *mrq) {
  char *rq = cgi_rq_string(mrq, "rq");
  if (str_eq(rq, "changePass")) {
    char *user = cgi_rq_string(mrq, "user");
    char *old = cgi_rq_string(mrq, "old");
    char *new = cgi_rq_string(mrq, "new");
    return cgi_change_pass(user, old, new);
  } else if (str_eq(rq, "close")) {
    char *sessionId = cgi_rq_string(mrq, "sessionId");
    cgi_remove_session(sessionId);
    return cgi_rp_empty();
  } else return FAIL(str_f("Unexpected value for 'rq': %s", rq));
}
