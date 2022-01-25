// Copyright 26-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "web/pointsPg.h"
#include "dmc/cgi.h"
#include "dmc/str.h"
#include "dmc/err.h"
#include "data/cts.h"
#include "db/points.h"
#include "db/teams.h"

char *pointsPg_process (Mchar *mrq) {
  char *rq = cgi_rq_string(mrq, "rq");
  if (str_eq(rq, "idata")) {
    AAOResult *points = points_read(cts_year());
    Mchar *rp = mchar_new();
    mchar_put(rp, "years", achar_to_js(teams_years()));
    mchar_put(rp, "teams", aKchar_to_js(mchar_to_array(cts_teams())));
    mchar_put(rp, "points", aAOResult_to_js(points));
    return cgi_rp(rp);
  } else if (str_eq(rq, "yearData")) {
    char *year = cgi_rq_string(mrq, "year");
    AKchar *teams = teams_read(year);
    AAOResult *results = points_read(year);
    Mchar *rp = mchar_new();
    mchar_put(rp, "teams", aKchar_to_js(teams));
    mchar_put(rp, "points", aAOResult_to_js(results));
    return cgi_rp(rp);
  } else return FAIL(str_f("Unexpected value for 'rq': %s", rq));
}
