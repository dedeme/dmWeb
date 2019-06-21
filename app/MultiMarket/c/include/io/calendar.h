// Copyright 08-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Management of 'calendar&#46;db'

#ifndef IO_CALENDAR_H
  #define IO_CALENDAR_H

#include "dmc/std.h"
#include "data/Timetable.h"
#include "DEFS.h"

///
void calendar_init (void);

/// Returns general time table of market
Timetable *calendar_general (void);

/// Sets general time table of market.
void calendar_set_general (Timetable *time_table);

/// Returns Arr[char]
Arr *calendar_holidays (void);

/// 'holidays' is Arr[char]
void calendar_set_holidays (Arr *holidays);

/// Returns Arr[MarketDay]
Arr *calendar_special_days (void);

/// 'special_days' is Arr[MarketDay]
void calendar_set_special_days (Arr *special_days);

///
int calendar_is_open (time_t date);

#endif
