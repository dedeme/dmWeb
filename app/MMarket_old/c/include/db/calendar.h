// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Management of 'calendar.db'

#ifndef DB_CALENDAR_H
  #define DB_CALENDAR_H

#include "dmc/async.h"
#include "data/Timetable.h"
#include "DEFS.h"

///
void calendar_init (void);

/// Returns general time table of market.
Timetable *calendar_general (void);

/// Sets general time table of market.
void calendar_set_general (Timetable *time_table);

/// Returns Arr[char].
Arr *calendar_holidays (void);

/// 'holidays' is Arr[char].
void calendar_set_holidays (Arr *holidays);

/// Returns Arr[MarketDay].
Arr *calendar_special_days (void);

/// 'special_days' is Arr[MarketDay].
void calendar_set_special_days (Arr *special_days);

/// Returns '1' if market is open.
int calendar_is_open (time_t date_time);

#endif
