// Copyright 24-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Bet conditions.

#ifndef DATA_BET_H
  #define DATA_BET_H

#include "data/cts.h"

/// Bet conditions.
struct bet_Bet {
  /// Payment by '1' result.
  double r1;
  /// Payment by 'x' result.
  double rx;
  /// Payment by '2' result.
  double r2;
};

/// Bet conditions.
typedef struct bet_Bet Bet;

///
Bet *bet_new (double r1, double rx, double r2);

/// Returns incomes from a bet_TYPE.
double bet_incomes (Bet *this, enum cts_BET_TYPE bet);

///
char *bet_to_js (Bet *this);

///
Bet *bet_from_js(char *js);

#endif
