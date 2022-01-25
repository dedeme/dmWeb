// Copyright 28-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Bet strategy.

#ifndef DATA_STRATEGY_H
  #define DATA_STRATEGY_H

#include "data/Result.h"
#include "data/Bet.h"
#include "data/Decision.h"

/// Bet stategy.
struct strategy_Strategy {
  /// Strategy identifier.
  char *(*id)(void);
  /// Strategy documentation.
  char *(*doc)(void);
  /// Returns bet decision.
  enum cts_BET_TYPE (*decision)(Result *result, Result *points, Bet *bet);
  /// Returns profits or decision with a bet of 1€.
  double (*profits)(Result *result, Result *points, Bet *bet);
};

/// Bet stategy.
typedef struct strategy_Strategy Strategy;

///
Strategy *strategy_new (
  char *(*id)(void),
  char *(*doc)(void),
  enum cts_BET_TYPE (*decision)(Result *result, Result *points, Bet *bet),
  double (*profits)(Result *result, Result *points, Bet *bet)
);

/// Returns a decision with a bet of 1€.
Decision *strategy_decide(
  Strategy *this, Result *result, Result *points, Bet *bet
);

/// Unimplemented.
char *strategy_to_js (Strategy *this);

/// Unimplemented.
Strategy *strategy_from_js (char *js);

#endif
