// Copyright 28-Nov-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef IO_RANK_H
  #define IO_RANK_H

#include "dmc/async.h"
#include "data/RankEntry.h"
#include "data/RankAssetsEntry.h"
#include "data/RankEvalEntry.h"

/// Initializes data base.
void rank_init (void);

/// Undate ranking database.
void rank_update (void);

/// Returns dates of historic ranking or throw an exception if the historic
/// ranking is empty.
///   return: Arr<char>. Dates in format "yyyymmdd" sorted ascendingly.
Arr *rank_dates (void);

/// Returns fleas of 'date' or
/// fleas of the last date if 'date' is not found or
/// throws an exception if last date is not found.
///   date  : Date to find.
///   return: Arr<RankAssetsEntry> Descendingly sorted
Arr *rank_fleas (char *date);

/// Returns fleas of day previous to 'date' or
/// fleas of day previous of the last date if 'date' is not found or
/// [] if no date is found.
///   date  : Date to find.
///   return: Arr<RankAssetsEntry>
Arr *rank_fleas_previous (char *date);

#endif
