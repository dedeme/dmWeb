// Copyright 24-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_DAILYENTRY_H
  #define DATA_DAILYENTRY_H

#include "dmc/async.h"

/*--*/

///
typedef struct DailyEntry_DailyEntry DailyEntry;

///
DailyEntry *dailyEntry_new(char *code, double close);

///
char *dailyEntry_code(DailyEntry *this);

///
double dailyEntry_close(DailyEntry *this);

/*--*/

#endif
