// Copyright 30-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "strategies/Everx.h"
#include "data/cts.h"

static char *id (void) {
  return "Ever x";
}

static char *doc (void) {
  return "Choose ever 'x'";
}

static enum cts_BET_TYPE decision (Result *result, Result *points, Bet *bet) {
  return cts_BET_x;
}

static double profits (Result *result, Result *points, Bet *bet) {
  int r = result_value(result);
  return r == cts_BET_x ? bet_incomes(bet, r) - 1 : -1;
}

Strategy *everx_mk (void) {
  return strategy_new(
    id,
    doc,
    decision,
    profits
  );
}
