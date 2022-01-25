// Copyright 28-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "web/profits/allStrategy.h"
#include "dmc/cgi.h"
#include "dmc/str.h"
#include "dmc/err.h"
#include "data/cts.h"
#include "strategies.h"
#include "db/teams.h"
#include "db/results.h"
#include "db/points.h"
#include "db/bets.h"

char *allStrategy_process (Mchar *mrq) {
  char *rq = cgi_rq_string(mrq, "rq");
  if (str_eq(rq, "idata")) {
    char *strategyId = cgi_rq_string(mrq, "strategy");
    Strategy *s = strategies_get(strategyId);
    AAAOResult *results = aAAOResult_new();
    AAAOResult *points = aAAOResult_new();
    AAAOBet *bets = aAAOBet_new();
    Achar *years = teams_years();
    char **yp = years->es;
    while (yp < years->end) {
      char *y = *yp++;
      aAAOResult_push(results, results_read(y));
      aAAOResult_push(points, points_read(y));
      aAAOBet_push(bets, bets_read(y));
    }
    AProfits *strategies = strategies_years_profits(
      s, years, results, points, bets
    );
    Mchar *rp = mchar_new();
    mchar_put(rp, "strategies", aProfits_to_js(strategies));
    mchar_put(rp, "sum", profits_to_js(aProfits_sum(strategies, "Total")));
    return cgi_rp(rp);
  } else return FAIL(str_f("Unexpected value for 'rq': %s", rq));
}
