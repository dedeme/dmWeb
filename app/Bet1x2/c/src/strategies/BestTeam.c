// Copyright 30-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "strategies/BestTeam.h"
#include "data/cts.h"

static char *id (void) {
  return "Team-Best";
}

static char *doc (void) {
  return "Choose the best team.";
}

static enum cts_BET_TYPE decision (Result *result, Result *points, Bet *bet) {
  double POND1 = 0.4336;
  double PONDx = 0.2901;

  double top = points->home + points->out;
  double sum = top * 2;
  double dif = points->home - points->out;

  double up = sum * POND1 - top;
  double down = sum * PONDx - top;
  return dif >= up ? cts_BET_1
    : dif >= down ? cts_BET_x
    : cts_BET_2
  ;
}

static double profits (Result *result, Result *points, Bet *bet) {
  enum cts_BET_TYPE d = decision(result, points, bet);

  enum cts_BET_TYPE r = result_value(result);
  return r == d ? bet_incomes(bet, r) - 1 : -1;
}

Strategy *bestTeam_mk (void) {
  return strategy_new(
    id,
    doc,
    decision,
    profits
  );
}
