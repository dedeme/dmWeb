// Copyright 28-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Strategy.h"
#include "dmc/err.h"

Strategy *strategy_new (
  char *(*id)(void),
  char *(*doc)(void),
  enum cts_BET_TYPE (*decision)(Result *result, Result *points, Bet *bet),
  double (*profits)(Result *result, Result *points, Bet *bet)
) {
  Strategy *this = MALLOC(Strategy);
  this->id = id;
  this->doc = doc;
  this->decision = decision;
  this->profits = profits;
  return this;
}

Decision *strategy_decide(
  Strategy *this, Result *result, Result *points, Bet *bet
) {
  return decision_new(
    result,
    points,
    bet,
    this->decision(result, points, bet)
  );
}

char *strategy_to_js (Strategy *this) {
  return FAIL ("Unimplemented");
}

/// Unimplemented.
Strategy *strategy_from_js (char *js) {
  return FAIL ("Unimplemented");
}
