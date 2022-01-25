// Copyright 24-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "web/main.h"
#include "dmc/cgi.h"
#include "dmc/str.h"
#include "dmc/err.h"
#include "dmc/date.h"
#include "data/cts.h"
#include "db/results.h"
#include "db/points.h"
#include "db/bets.h"
#include "db/teams.h"

char *main_process (Mchar *mrq) {
  char *rq = cgi_rq_string(mrq, "rq");
  if (str_eq(rq, "close")) {
    char *sessionId = cgi_rq_string(mrq, "sessionId");
    cgi_remove_session(sessionId);
    return cgi_rp_empty();
  } else return FAIL(str_f("Unexpected value for 'rq': %s", rq));
}
