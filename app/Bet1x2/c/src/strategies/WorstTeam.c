// Copyright 30-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "strategies/WorstTeam.h"
#include "data/cts.h"

static char *id (void) {
  return "Team-Worst";
}

static char *doc (void) {
  return "Choose the worst team.";
}

static enum cts_BET_TYPE decide (Result *result, Result *points, Bet *bet) {
  double POND1 = 0.4336;
  double PONDx = 0.2901;

  double top = points->home + points->out;
  double sum = top * 2;
  double dif = points->home - points->out;

  double up = sum * POND1 - top;
  double down = sum * PONDx - top;
  return dif >= up ? cts_BET_2
    : dif >= down ? cts_BET_x
    : cts_BET_1
  ;
}

static double profits (Result *result, Result *points, Bet *bet) {
  enum cts_BET_TYPE d = decide(result, points, bet);
  enum cts_BET_TYPE r = result_value(result);
  return r == d ? bet_incomes(bet, r) - 1 : -1;
}

Strategy *worstTeam_mk (void) {
  return strategy_new(
    id,
    doc,
    decide,
    profits
  );
}
