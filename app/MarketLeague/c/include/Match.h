// Copyright 24-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Match data

#ifndef MATCH_H
  #define MATCH_H

#include "dmc/std.h"

/*--*/

///
///   Arguments:
///     up: int
///     down: int
typedef struct Match_Match Match;

///
Match *match_new (int up, int down);

///
int match_up (Match *this);

///
int match_down (Match *this);

/*--*/

/// Returns Arr[Arr[Match]].
///   players: Players number. Must be even. arr_size of return is players - 1.
///            Each array entry is an array of players / 2
Arr *match_rounds (int players);

#endif
