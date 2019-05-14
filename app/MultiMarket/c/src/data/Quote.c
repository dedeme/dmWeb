// Copyright 07-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Quote.h"
#include "dmc/Dec.h"
#include "errno.h"

/* .
-Quote
  date: char *
  open: double
  close: double
  max: double
  min: double
  vol: int
  # It it is 1, quotes are handly modified.
  error: bool
*/
/*--*/

struct Quote_Quote{
  char *date;
  double open;
  double close;
  double max;
  double min;
  int vol;
  int error;
};

static Quote *_quote_new(
  char *date,
  double open,
  double close,
  double max,
  double min,
  int vol,
  int error
) {
  Quote *this = MALLOC(Quote);
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
  return this->date;
}

double quote_open(Quote *this) {
  return this->open;
}

double quote_close(Quote *this) {
  return this->close;
}

double quote_max(Quote *this) {
  return this->max;
}

double quote_min(Quote *this) {
  return this->min;
}

int quote_vol(Quote *this) {
  return this->vol;
}

int quote_error(Quote *this) {
  return this->error;
}

/*--*/

char *quote_test(Quote *this) {
  if (this->error) return quote_OK;
  if (this->open > this->max) return quote_OMAX;
  if (this->close > this->max) return quote_CMAX;
  if (this->open < this->min) return quote_OMIN;
  if (this->close < this->min) return quote_CMIN;
  return quote_OK;
}

char *quote_test2 (Quote *this, Quote *q_model, Quote *previous_q) {
  if (this->error) return quote_OK;
  int cmp = strcmp(this->date, q_model->date);
  if (cmp > 0) return quote_NO_DATE;
  if (cmp < 0) return quote_EXTRA_DATE;
  if (
    this->open > previous_q->open * 1.20 ||
    this->open < previous_q->open * 0.8
  ) return quote_OPEN20;
  if (
    this->close > previous_q->close * 1.20 ||
    this->close < previous_q->close * 0.8
  ) return quote_CLOSE20;
  if (
    this->max > previous_q->max * 1.20 ||
    this->max < previous_q->max * 0.8
  ) return quote_MAX20;
  if (
    this->min > previous_q->min * 1.20 ||
    this->min < previous_q->min * 0.8
  ) return quote_MIN20;

  return quote_OK;
}

Opt *quote_from_str (char *q) {
  char **parts = (char **)arr_start(str_csplit(q, ':'));
  char *tail;
  errno = 0;

  char *date = parts[0];
  if (strlen(date) != 8 || !dec_digits(date)) return opt_empty();
  double open = strtod(parts[1], &tail);
  if (errno || *tail) return opt_empty();
  double close = strtod(parts[2], &tail);
  if (errno || *tail) return opt_empty();
  double max = strtod(parts[3], &tail);
  if (errno || *tail) return opt_empty();
  double min = strtod(parts[4], &tail);
  if (errno || *tail) return opt_empty();
  char *v = parts[5];
  if (*v == '-') v = v + 1;
  if (!dec_digits(v)) return opt_empty();
  int vol = atoi(parts[5]);
  int error = 0;
  if (str_eq(parts[6], "true")) error = 1;
  else if (!str_eq(parts[6], "false")) return opt_empty();

  return opt_new(_quote_new(date, open, close, max, min, vol, error));
}

char *quote_to_str (Quote *q) {
  return str_f(
    "%s:%.4f:%.4f:%.4f:%.4f:%d:%s",
    q->date, q->open, q->close, q->max, q->min, q->vol,
    q->error ? "true" : "false"
  );
}
