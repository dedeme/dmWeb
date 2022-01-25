// Copyright 24-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "web/home.h"
#include "dmc/cgi.h"
#include "dmc/str.h"
#include "dmc/err.h"
#include "data/cts.h"
#include "data/Strategy/AStrategy.h"
#include "strategies.h"
#include "db/teams.h"
#include <stdio.h>

char *home_process (Mchar *mrq) {
  char *rq = cgi_rq_string(mrq, "rq");
  if (str_eq(rq, "idata")) {
    /**/void *to_doc(Strategy *s) { return kchar_new(s->id(), s->doc()); }
    Mchar *doc = mchar_from_array(
      (AKchar *)aStrategy_map(strategies_list(), to_doc)
    );
    Mchar *rp = mchar_new();
    mchar_put(rp, "years", achar_to_js(teams_years()));
    mchar_put(rp, "doc", mchar_to_js(doc));
    return cgi_rp(rp);
  } else return FAIL(str_f("Unexpected value for 'rq': %s", rq));
  return FAIL(str_f("Unexpected value for 'rq': %s", rq));
}
