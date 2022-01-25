// Copyright 28-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Strategies list.

#ifndef STRATEGIES_H
  #define STRATEGIES_H

#include "data/Strategy/AStrategy.h"
#include "data/Profits/AProfits.h"
#include "data/Result/AAAOResult.h"
#include "data/Bet/AAAOBet.h"

/// Unsorted list of bet strategies.
AStrategy *strategies_list(void);

/// Returna the strategy with identifier 'id'.
Strategy *strategies_get (char *id);

/// Calculate profits of a year.
///   this       : Strategy to use.
///   description: Profits description.
///   results    : Year results.
///   points     : Year points.
///   bets       : Year bets.
Profits *strategies_year_profits (
  Strategy *s,
  char *description,
  AAOResult *results,
  AAOResult *points,
  AAOBet *bets
);

/// Calculate profites of serveral years. Each return mach a year.
/// The return is not ordered.
///   this   : Strategy to use.
///   years  : List of years.
///   results: Several years results, one for each year of 'years' and
///            matching 'years' order.
///   points: Several years points, one for each year of 'years' and
///            matching 'years' order.
///   bets  : Several years bets, one for each year of 'years' and
///            matching 'years' order.
AProfits *strategies_years_profits (
  Strategy *s,
  Achar *years,
  AAAOResult *results,
  AAAOResult *points,
  AAAOBet *bets
);

/// Calculate profits of a year for a group of strategies.
///   ss: Strategies to use.
///   results    : Year results.
///   points     : Year points.
///   bets       : Year bets.
AProfits *strategies_year_group_profits (
  AStrategy *ss,
  AAOResult *results,
  AAOResult *points,
  AAOBet *bets
);

/// Calculate profits of several years for a group of strategies.
/// Returns a Profit for each strategy.
///   ss: Strategies to use.
///   results: Several years results, one for each year of 'years' and
///            matching years order of points and bets.
///   points: Several years points, one for each year of 'years' and
///            matching years order of results and bets.
///   bets  : Several years bets, one for each year of 'years' and
///            matching years order of results and points.
AProfits *strategies_years_group_profits (
  AStrategy *ss,
  AAAOResult *results,
  AAAOResult *points,
  AAAOBet *bets
);


#endif
