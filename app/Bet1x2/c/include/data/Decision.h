// Copyright 28-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Decision data.

#ifndef DATA_DECISION_H
  #define DATA_DECISION_H

#include "data/Result.h"
#include "data/Bet.h"

/// Decision data.
struct decision_Decision {
  Result *result;
  Result *points;
  Bet *bet;
  enum cts_BET_TYPE decision;
};

/// Decision data.
typedef struct decision_Decision Decision;

///
Decision *decision_new(
  Result *result, Result *points, Bet *bet, enum cts_BET_TYPE decision
);

///
char *decision_to_js (Decision *this);

///
Decision *decision_from_js(char *js);

#endif
