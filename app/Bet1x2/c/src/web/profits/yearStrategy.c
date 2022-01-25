// Copyright 28-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "web/profits/yearStrategy.h"
#include "dmc/cgi.h"
#include "dmc/str.h"
#include "dmc/err.h"
#include "strategies.h"
#include "data/cts.h"
#include "strategies.h"
#include "data/Decision/AAODecision.h"
#include "db/results.h"
#include "db/points.h"
#include "db/bets.h"

static AAODecision *decs (
  char *year,
  Strategy *s,
  AAOResult *results,
  AAOResult *points,
  AAOBet *bets
) {
  int size = mchar_size(cts_teams());

  AAODecision *r = aAODecision_new();
  for (int irow = 0; irow < size; ++irow) {
    AODecision *row = aODecision_new();
    for (int icol = 0; icol < size; ++icol) {
      Result *rs = oResult_nsome(
        aOResult_get(aAOResult_get(results, irow), icol)
      );
      Result *pts = oResult_nsome(
        aOResult_get(aAOResult_get(points, irow), icol)
      );
      Bet *bet = oBet_nsome(
        aOBet_get(aAOBet_get(bets, irow), icol)
      );

      if (rs && pts && bet) {
        aODecision_push(
          row,
          oDecision_mk_some(strategy_decide(s, rs, pts, bet))
        );
      } else {
        aODecision_push(row, oDecision_mk_none());
      }
    }
    aAODecision_push(r, row);
  }

  return r;
}

char *yearStrategy_process (Mchar *mrq) {
  char *rq = cgi_rq_string(mrq, "rq");
  if (str_eq(rq, "idata")) {
    char *year = cgi_rq_string(mrq, "year");
    Strategy *strategy = strategies_get(cgi_rq_string(mrq, "strategy"));
    AAOResult *results = results_read(year);
    AAOResult *points = points_read(year);
    AAOBet *bets = bets_read(year);
    Profits *pfs = strategies_year_profits(
      strategy, strategy->id(), results, points, bets
    );
    Mchar *rp = mchar_new();
    mchar_put(rp, "teams", aKchar_to_js(mchar_to_array(cts_teams())));
    mchar_put(rp, "decisions", aAODecision_to_js(decs(
      year, strategy, results, points, bets
    )));
    mchar_put(rp, "profits", profits_to_js(pfs));

    return cgi_rp(rp);
  } else return FAIL(str_f("Unexpected value for 'rq': %s", rq));
}
