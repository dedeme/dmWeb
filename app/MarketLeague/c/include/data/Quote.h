// Copyright 28-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Quote data.

#ifndef DATA_QUOTE_H
  #define DATA_QUOTE_H

#include "dmc/std.h"

/*--*/

///
///   Arguments:
///     date: time_t
///     open: double
///     close: double
///     max: double
///     min: double
///     vol: int
typedef struct Quote_Quote Quote;

///
Quote *quote_new (
  time_t date,
  double open,
  double close,
  double max,
  double min,
  int vol
);

///
time_t quote_date (Quote *this);

///
double quote_open (Quote *this);

///
double quote_close (Quote *this);

///
double quote_max (Quote *this);

///
double quote_min (Quote *this);

///
int quote_vol (Quote *this);

/*--*/

/// Arr<Quote> Returns quotes from nick file data. Quotes are from after to
/// before.
Arr *quote_table (char *data);

#endif
