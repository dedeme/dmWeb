// Copyright 22-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Fleas general ranking.

#ifndef DB_FLEAS_RANKING_H
  #define DB_FLEAS_RANKING_H

#include "dmc/async.h"

///
void ranking_init ();

/// Arr[Investor]. Returns pool of general ranking.
Arr *ranking_read_pool (void);

/// Writes pool of general ranking. 'investors' is Arr[Investor]
void ranking_write_pool (Arr *investors);

/// Arr[Arr[Investor]]. Returns general ranking.
/// 10 last ranking (Array 10 (days) of 40 (fleas)).
/// Every Arr[Investor] is descendently ordered.
Arr *ranking_read (void);

/// Writes general ranking. 'ranking' is Arr[Arr[Investor]].
/// 10 last ranking (Array 10 (days) of 40 (fleas)).
/// Every Arr[Investor] is descendently ordered.
void ranking_write (Arr *ranking);

#endif
