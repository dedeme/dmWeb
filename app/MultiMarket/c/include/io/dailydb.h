// Copyright 28-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Daily companies data.

#ifndef IO_DAILYDB_H
  #define IO_DAILYDB_H

#include "dmc/async.h"

///
void dailydb_init (void);

/// Called when activating
void dailydb_reset (void);

/// Called when activating (after reset) and when active
void dailydb_update (void);

/// Returns daily data of selected companies. Data is:
///   [
///     [
///       (0) Nick: String -- Company nick name
///       (1) Quotes: [
///             [
///               (0) Hour: String -- Quote hour
///               (1) Quote: Double -- Retrieved from accdb_dailyq_read
///             ] -- One for each valid quote reading (values < 1 are discarded)
///           ]
///       (2) Stocks: Double -- Stocks in portfolio or 0
///       (3) Price: Double -- Buy price or 0
///       (4) ref: Double -- Buy-Sell signal (reference)
///                          (> 0) Support (buying)
///                          (< 0) -> Resitence (selling)
///                          (= 0) -> Not operation
///    ] -- One for each selected company
///  ]
Js *dailydb_cos (void);

/// Add-remove companies and update its inclusion in portfolio.
void dailydb_update_charts (void);

#endif
