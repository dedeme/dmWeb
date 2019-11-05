// Copyright 27-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Quotes data base.

#ifndef IO_QUOTES_H
  #define IO_QUOTES_H

#include "dmc/std.h"
#include "dmc/MatchRsRound.h"
#include "dmc/MatchRound.h"

/// Arr<char> Returns best nicks from the first quote to 'date', sorted
/// from better to worse.
Arr *quotes_bests (time_t date);

/// Opt<MatchRsRound> Returns results of 'date'. If date has not quotes returns
/// opt_empty()
Opt *quotes_results(time_t date, Arr *nicks, MatchRound *round);

/// Opt<MatchRsRound> Returns results of last date. If it has not quotes returns
/// opt_empty()
Opt *quotes_last_results(Arr *nicks, MatchRound *round);

/// Returns Arr<char> Nicks filtered by volume.
///   month: Month in which league start.
///   year: Year in which league start.
Arr *quotes_vol_filter (int month, int year);

/// Returns Arr<char> Nicks filtered by volume.
///   start: Date to start inclusive.
///   end: date to end exclusive.
Arr *quotes_vol_filter2 (time_t start, time_t end);

/// Returns the last date in quotes historic.
time_t quotes_last_date (void);

/// Returns Arr<Quote> - The last quotes in model quotes historic.
Arr *quotes_last_quotes (void);

/// Returns if market is open now.
int quotes_is_market_open (void);

#endif
