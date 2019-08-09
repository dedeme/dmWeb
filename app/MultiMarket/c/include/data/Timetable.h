// Copyright 08-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Time table of market open-close.

#ifndef DATA_TIMETABLE_H
  #define DATA_TIMETABLE_H

#include "dmc/async.h"

/*--*/

///
///   Arguments:
///     hopen: int
///     mopen: int
///     hclose: int
///     mclose: int
typedef struct Timetable_Timetable Timetable;

///
int timetable_hopen (Timetable *this);

///
int timetable_mopen (Timetable *this);

///
int timetable_hclose (Timetable *this);

///
int timetable_mclose (Timetable *this);

///
Js *timetable_to_js (Timetable *this);

///
Timetable *timetable_from_js (Js *js);

/*--*/

/// Creates a time table with values: hopen = 0, mopen = 0, hclose = 23,
/// mclose = 55
Timetable *timetable_new(void);
#endif
