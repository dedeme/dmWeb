// Copyright 08-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Date and time table of holidays and special days.

#ifndef DATA_MARKETDAY_H
  #define DATA_MARKETDAY_H

#include "dmc/std.h"

/*--*/

///
typedef struct MarketDay_MarketDay MarketDay;

///
char *marketDay_date(MarketDay *this);

///
int marketDay_hopen(MarketDay *this);

///
int marketDay_mopen(MarketDay *this);

///
int marketDay_hclose(MarketDay *this);

///
int marketDay_mclose(MarketDay *this);

///
Js *marketDay_to_js(MarketDay *this);

///
MarketDay *marketDay_from_js(Js *js);

/*--*/

#endif
