// Copyright 10-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_QUOTE_H
  #define DATA_QUOTE_H

#include "dmc/std.h"

/*.-.*/

///
typedef struct quote_Quote Quote;

///
Quote *quote_new(
  char *date,
  double open,
  double close,
  double max,
  double min,
  int vol,
  bool error
);

///
char *quote_date(Quote *this);

///
double quote_open(Quote *this);

///
double quote_close(Quote *this);

///
double quote_max(Quote *this);

///
double quote_min(Quote *this);

///
int quote_vol(Quote *this);

///
bool quote_error(Quote *this);

/*.-.*/

#define TY Quote
#define FN quote
#include "dmc/tpl/topt.h"
#include "dmc/tpl/tarr.h"
#include "dmc/tpl/tit.h"
#undef TY
#undef FN

#define TY Aquote
#define FN aquote
#include "dmc/tpl/topt.h"
#undef TY
#undef FN

///
Quote *quote_from_str(char *q);

///
char *quote_to_str(Quote *this);


#endif

