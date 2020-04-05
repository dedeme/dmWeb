// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Entry record for daily data read from server.

#ifndef DATA_DAILY_H
  #define DATA_DAILY_H

#include "dmc/async.h"

/*--*/

/// Record for daily data read from server.
///   Arguments:
///     code: char*
///     close: double
typedef struct Daily_DailyEntry DailyEntry;

///
DailyEntry *dailyEntry_new (char *code, double close);

///
char *dailyEntry_code (DailyEntry *this);

///
double dailyEntry_close (DailyEntry *this);

/*--*/

#endif
