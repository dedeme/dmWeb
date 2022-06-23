// Copyright 30-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "strategies/Ever2.h"
#include "data/cts.h"

static char *id (void) {
  return "Ever 2";
}

static char *doc (void) {
  return "Choose ever '2'";
}

static enum cts_BET_TYPE decision (Result *result, Result *points, Bet *bet) {
  return cts_BET_2;
}

static double profits (Result *result, Result *points, Bet *bet) {
  int r = result_value(result);
  return r == cts_BET_2 ? bet_incomes(bet, r) - 1 : -1;
}

Strategy *ever2_mk (void) {
  return strategy_new(
    id,
    doc,
    decision,
    profits
  );
}