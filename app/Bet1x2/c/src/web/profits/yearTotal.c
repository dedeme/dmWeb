// Copyright 28-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "web/profits/yearTotal.h"
#include "dmc/cgi.h"
#include "dmc/str.h"
#include "dmc/err.h"
#include "data/cts.h"
#include "strategies.h"
#include "db/results.h"
#include "db/points.h"
#include "db/bets.h"

char *yearTotal_process (Mchar *mrq) {
  char *rq = cgi_rq_string(mrq, "rq");
  if (str_eq(rq, "idata")) {
    char *year = cgi_rq_string(mrq, "year");
    AStrategy *ss = strategies_list();
    AAOResult *results = results_read(year);
    AAOResult *points =  points_read(year);
    AAOBet *bets =bets_read(year);
    AProfits *strategies = strategies_year_group_profits(
      ss, results, points, bets
    );
    Mchar *rp = mchar_new();
    mchar_put(rp, "strategies", aProfits_to_js(strategies));
    return cgi_rp(rp);
  } else return FAIL(str_f("Unexpected value for 'rq': %s", rq));
}
