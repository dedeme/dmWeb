// Copyright 25-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Company mark after a league

#ifndef MARK_H
  #define MARK_H

#include "dmc/std.h"
#include "Data.h"

/*--*/

///
///   Arguments:
///     nick: char*
///     points: int
///     dif: double
typedef struct Mark_Mark Mark;

///
Mark *mark_new (char *nick, int points, double dif);

///
char *mark_nick (Mark *this);

///
int mark_points (Mark *this);

///
double mark_dif (Mark *this);

/*--*/

/// Returns Arr[char] A ordered nick list (from winners to lossers) after
/// playing 'league'
Arr *mark_ranking (DataLeague *league);

#endif
