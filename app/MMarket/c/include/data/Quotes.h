// Copyright 15-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Companies quotes.

#ifndef DATA_QUOTES_H
  #define DATA_QUOTES_H

#include "dmc/char/Achar.h"
#include "data/ADouble/AADouble.h"

/// Companies quotes.
struct quotes_Quotes {
  // Date in which data was read.
  char *date;
  //Company nicks, sorted.
  Achar *cos;
  // Dates in format YYYYMMDD, from before to after.
  Achar *dates;
  // Matrix of quotes opens.
  // Its rows match 'dates' and is columns 'cos'.
  AADouble *opens;
  // Matrix of quotes closes.
  // Its rows match 'dates' and is columns 'cos'.
  AADouble *closes;
  // Matrix of maximum closes.
  // Its rows match 'dates' and is columns 'cos'.
  AADouble *maxs;
};

/// Companies quotes.
typedef struct quotes_Quotes Quotes;

/// Constructor
///   date  : Date in which data was read.
///   cos   : Company nicks, sorted.
///   dates : Dates in format YYYYMMDD, from before to after.
///   opens : Matrix of quotes opens.
///           Its rows match 'dates' and is columns 'cos'.
///   closes: Matrix of quotes closes.
///           Its rows match 'dates' and is columns 'cos'.
///   maxs  : Matrix of maximun quotes.
///           Its rows match 'dates' and is columns 'cos'.
Quotes *quotes_new (
  char *date, Achar *cos, Achar *dates,
  AADouble *opens, AADouble *closes, AADouble *maxs
);

/// Returns company index or -1.
///   this:
///   co  : Company name.
int quotes_ico(Quotes *this, char *co);

/// Returns closes of a company.
///   this:
///   ico : Company index as result of 'quotes_ico'.
ADouble *quotes_closes(Quotes *this, int ico);

/// Returns maximun quotes of a company.
///   this:
///   ico : Company index as result of 'quotes_ico'.
ADouble *quotes_maxs(Quotes *this, int ico);

/// Returns closes of a company in format 'Quotes'.
///   this:
///   ico : Company index as result of 'quotes_ico'.
Quotes *quotes_mk_single(Quotes *this, int ico);

///
char *quotes_to_js(Quotes *this);

///
Quotes *quotes_from_js(char *js);

#endif
