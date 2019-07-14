// Copyright 24-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Entry for daily data read from server.

#ifndef DATA_DAILYENTRY_H
  #define DATA_DAILYENTRY_H

#include "dmc/std.h"

/*--*/

///
///   Arguments:
///     code: char*
///     close: double
typedef struct DailyEntry_DailyEntry DailyEntry;

///
DailyEntry *dailyEntry_new (char *code, double close);

///
char *dailyEntry_code (DailyEntry *this);

///
double dailyEntry_close (DailyEntry *this);

/*--*/

#endif
