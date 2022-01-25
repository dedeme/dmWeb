// Copyright 30-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "strategies/BestIncomes.h"
#include "data/cts.h"

static char *id (void) {
  return "Incomes-Best";
}

static char *doc (void) {
  return "Choose the bet option which produces maximum incomes.";
}

static enum cts_BET_TYPE decision (Result *result, Result *points, Bet *bet) {
  return bet->r1 > bet->rx && bet->r1 > bet->r2 ? cts_BET_1
    : bet->rx > bet->r2 ? cts_BET_x
    : cts_BET_2
  ;
}

static double profits (Result *result, Result *points, Bet *bet) {
  enum cts_BET_TYPE d = decision(result, points, bet);
  enum cts_BET_TYPE r = result_value(result);
  return r == d ? bet_incomes(bet, r) - 1 : -1;
}

Strategy *bestIncomes_mk (void) {
  return strategy_new(
    id,
    doc,
    decision,
    profits
  );
}
