// Copyright 28-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Quote.h"
#include "dmc/date.h"

/* .
Quote
  date: time_t
  open: double
  close: double
  max: double
  min: double
  vol: int
*/

/*--*/

struct Quote_Quote {
  time_t date;
  double open;
  double close;
  double max;
  double min;
  int vol;
};

Quote *quote_new (
  time_t date,
  double open,
  double close,
  double max,
  double min,
  int vol
) {
  Quote *this = MALLOC(Quote);
  this->date = date;
  this->open = open;
  this->close = close;
  this->max = max;
  this->min = min;
  this->vol = vol;
  return this;
}

time_t quote_date (Quote *this) {
  return this->date;
}

double quote_open (Quote *this) {
  return this->open;
}

double quote_close (Quote *this) {
  return this->close;
}

double quote_max (Quote *this) {
  return this->max;
}

double quote_min (Quote *this) {
  return this->min;
}

int quote_vol (Quote *this) {
  return this->vol;
}

/*--*/

Arr *quote_table (char *data) {
  // Arr<Quote>
  Arr *r = arr_new();
  EACH(str_csplit(data, '\n'), char, record) {
    // Arr<char>
    Arr *fs = str_csplit(record, ':');
    arr_push(r, quote_new(
      date_from_str(arr_get(fs, 0)),
      js_rd(arr_get(fs, 1)),
      js_rd(arr_get(fs, 2)),
      js_rd(arr_get(fs, 3)),
      js_rd(arr_get(fs, 4)),
      js_ri(arr_get(fs, 5))
    ));
  }_EACH
  return r;
}
