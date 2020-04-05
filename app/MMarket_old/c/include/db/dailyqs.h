// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Daily quotes database.

#ifndef DB_DAILYQS_H
  #define DB_DAILYQS_H

#include "dmc/async.h"
#include "data/Qtable.h"

///
void dailyqs_init (void);

/// Returns a QtableRow of quotes, which indices match nicks indices. Values
/// missing are set to '-1'.
/// 'nicks' is Arr[char]
QtableRow dailyqs_read (Arr *nicks);

/// if 'nick' is missing, returns -1
double dailyqs_read_nick (char *nick);

/// 'quotes' is Js-> Map[Js->double]
void dailyqs_write (Js *quotes);


#endif
