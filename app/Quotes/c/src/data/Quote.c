// Copyright 10-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Quote.h"

/* .+.
-struct: Quote
  date: char *
  open: double
  close: double
  max: double
  min: double
  vol: int
  error: bool
*/
/*.-.*/
#include "dmc/ct/Ajson.h"

struct quote_Quote {
  char *date;
  double open;
  double close;
  double max;
  double min;
  int vol;
  bool error;
};

Quote *quote_new(
  char *date,
  double open,
  double close,
  double max,
  double min,
  int vol,
  bool error
) {
  Quote *this = MALLOC(Quote);
  XNULL(date)
  this->date = date;
  this->open = open;
  this->close = close;
  this->max = max;
  this->min = min;
  this->vol = vol;
  this->error = error;
  return this;
}

char *quote_date(Quote *this) {
  XNULL(this)
  return this->date;
}

double quote_open(Quote *this) {
  XNULL(this)
  return this->open;
}

double quote_close(Quote *this) {
  XNULL(this)
  return this->close;
}

double quote_max(Quote *this) {
  XNULL(this)
  return this->max;
}

double quote_min(Quote *this) {
  XNULL(this)
  return this->min;
}

int quote_vol(Quote *this) {
  XNULL(this)
  return this->vol;
}

bool quote_error(Quote *this) {
  XNULL(this)
  return this->error;
}
/*.-.*/

#define TY Quote
#define FN quote
#include "dmc/tpl/topt.c"
#include "dmc/tpl/tarr.c"
#undef TY
#undef FN

#define TY Aquote
#define FN aquote
#include "dmc/tpl/topt.c"
#undef TY
#undef FN


Quote *quote_from_str(char *q) {
  Achar *parts = str_csplit(q, ':');
  return quote_new(
    achar_get(parts, 0),
    atof(achar_get(parts, 1)),
    atof(achar_get(parts, 2)),
    atof(achar_get(parts, 3)),
    atof(achar_get(parts, 4)),
    atoi(achar_get(parts, 5)),
    str_eq(achar_get(parts, 6), "true") ? true : false
  );
}

char *quote_to_str(Quote *this) {
  return str_printf("%s:%.4f:%.4f:%.4f:%.4f:%d:%s",
    this->date,
    this->open,
    this->close,
    this->max,
    this->min,
    this->vol,
    this->error ? "true" : "false"
  );
}

