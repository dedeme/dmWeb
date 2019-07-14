// Copyright 07-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Quote.h"
#include "dmc/Dec.h"
#include "errno.h"
#include "DEFS.h"

/* .
Quote: serial
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

struct Quote_Quote {
  char *date;
  double open;
  double close;
  double max;
  double min;
  int vol;
  int error;
};

Quote *quote_new (
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

char *quote_date (Quote *this) {
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

int quote_error (Quote *this) {
  return this->error;
}

Js *quote_to_js (Quote *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(this->date));
  arr_push(js, js_wd(this->open));
  arr_push(js, js_wd(this->close));
  arr_push(js, js_wd(this->max));
  arr_push(js, js_wd(this->min));
  arr_push(js, js_wi((int)this->vol));
  arr_push(js, js_wb(this->error));
  return js_wa(js);
}

Quote *quote_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  Quote *this = MALLOC(Quote);
  this->date = js_rs(*p++);
  this->open = js_rd(*p++);
  this->close = js_rd(*p++);
  this->max = js_rd(*p++);
  this->min = js_rd(*p++);
  this->vol = js_ri(*p++);
  this->error = js_rb(*p++);
  return this;
}

/*--*/

Opt *quote_from_str (char *q) {
  // Arr[char]
  Arr *a = str_csplit(q, ':');
  if (arr_size(a) != 7) {
    return opt_empty();
  }
  char **parts = (char **)arr_start(a);
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

  return opt_new(quote_new(date, open, close, max, min, vol, error));
}

char *quote_to_str (Quote *q) {
  return str_f(
    "%s:%.4f:%.4f:%.4f:%.4f:%d:%s",
    q->date, q->open, q->close, q->max, q->min, q->vol,
    q->error ? "true" : "false"
  );
}

// 'aqs' is Arr[Arr[Quote]]. 'best_qs' and return are Arr[Quote]
Arr *quote_unify (Arr *aqs, Arr *best_qs) {
  int size = arr_size(aqs);
  int even = size % 2 == 0;
  int mid = even ? size / 2 : (size + 1) / 2;

  // Arr[Quote]
  Arr *r = arr_new();

  // Arr[It[Quote]]
  Arr *aits = arr_new();
  EACH(aqs, Arr, a)
    arr_push(aits, arr_to_it(a));
  _EACH
  // It[Quote]
  It *best_it = arr_to_it(best_qs);

  // ------------------------------------------------------------------------ //
  int has_next () {                                                           //
    EACH(aits, It, it)                                                        //
      if (it_has_next(it)) return 1;                                          //
    _EACH                                                                     //
    return 0;                                                                 //
  }                                                                           //
  // ------------------------------------------------------------------------ //

  while (has_next()) {
    Quote *best_q = NULL;
    if (it_has_next(best_it)) {
      best_q = it_peek(best_it);
    } else {
      EACH(aits, It, it)
        if (it_has_next(it)) {
          best_q = it_peek(it);
          break;
        }
      _EACH
    }

    char *best_date = quote_date(best_q);
    char *max_date = best_date;
    EACH(aits, It, it)
      if (
        it_has_next(it) &&
        strcmp(quote_date(it_peek(it)), max_date) > 0
      ) {
        max_date = quote_date(it_peek(it));
        best_q = it_peek(it);
      }
    _EACH

    if (str_eq(best_date, max_date) && it_has_next(best_it)) {
      it_next(best_it);
    }

    // Arr[Quote]
    Arr *sels = arr_new();
    EACH(aits, It, it)
      if (
        it_has_next(it) &&
        str_eq(quote_date(it_peek(it)), max_date)
      ) {
        arr_push(sels, it_next(it));
      }
    _EACH


    int sels_size = arr_size(sels);
    if (
      sels_size > mid ||
      (sels_size == mid && !even) ||
      (sels_size == mid && str_eq(best_date, max_date))
    ) {
      // -------------------------------------------------------------------- //
      double max_double (double *ns, double nsel) {                           //
        Arr *scores = arr_new();                                              //
        RANGE0(i, sels_size)                                                  //
          int missing = 1;                                                    //
          double n = ns[i];                                                   //
          EACH(scores, Tp, n_sc)                                              //
            if (*(double *)tp_e1(n_sc) == n) {                                //
              *(int *)tp_e2(n_sc) += 1;                                       //
              missing = 0;                                                    //
              break;                                                          //
            }                                                                 //
          _EACH                                                               //
          if (missing) {                                                      //
            double *pn = ATOMIC(sizeof(double));                              //
            *pn = n;                                                          //
            int *sc = ATOMIC(sizeof(int));                                    //
            *sc = 0;                                                          //
            Tp *n_sc = tp_new(pn, sc);                                        //
            arr_push(scores, n_sc);                                           //
          }                                                                   //
        _RANGE                                                                //
                                                                              //
        Tp *sel_n_sc = arr_get(scores, 0);                                    //
        EACH(scores, Tp, n_sc)                                                //
          int sel_sc = *(int *)tp_e2(sel_n_sc);                               //
          double n = *(double *)tp_e1(n_sc);                                  //
          int sc = *(int *)tp_e2(n_sc);                                       //
          if (                                                                //
            sc > sel_sc ||                                                    //
            (sc == sel_sc && n == nsel)                                       //
          ) {                                                                 //
            sel_n_sc = n_sc;                                                  //
          }                                                                   //
        _EACH                                                                 //
                                                                              //
        return *(double *)tp_e1(sel_n_sc);                                    //
      }                                                                       //
      // -------------------------------------------------------------------- //

      // -------------------------------------------------------------------- //
      int max_int (int *ns, int nsel) {                                       //
        Arr *scores = arr_new();                                              //
        RANGE0(i, sels_size)                                                  //
          int missing = 1;                                                    //
          int n = ns[i];                                                      //
          EACH(scores, Tp, n_sc)                                              //
            if (*(int *)tp_e1(n_sc) == n) {                                   //
              *(int *)tp_e2(n_sc) += 1;                                       //
              missing = 0;                                                    //
              break;                                                          //
            }                                                                 //
          _EACH                                                               //
          if (missing) {                                                      //
            int *pn = ATOMIC(sizeof(int));                                    //
            *pn = n;                                                          //
            int *sc = ATOMIC(sizeof(int));                                    //
            *sc = 0;                                                          //
            Tp *n_sc = tp_new(pn, sc);                                        //
            arr_push(scores, n_sc);                                           //
          }                                                                   //
        _RANGE                                                                //
                                                                              //
        Tp *sel_n_sc = arr_get(scores, 0);                                    //
        EACH(scores, Tp, n_sc)                                                //
          int sel_sc = *(int *)tp_e2(sel_n_sc);                               //
          int n = *(int *)tp_e1(n_sc);                                        //
          int sc = *(int *)tp_e2(n_sc);                                       //
          if (                                                                //
            sc > sel_sc ||                                                    //
            (sc == sel_sc && n == nsel)                                       //
          ) {                                                                 //
            sel_n_sc = n_sc;                                                  //
          }                                                                   //
        _EACH                                                                 //
                                                                              //
        return *(int *)tp_e1(sel_n_sc);                                       //
      }                                                                       //
      // -------------------------------------------------------------------- //

      double *opens = ATOMIC(sels_size * sizeof(double));
      EACH_IX(sels, Quote, q, i)
        opens[i] = q->open;
      _EACH
      double *closes = ATOMIC(sels_size * sizeof(double));
      EACH_IX(sels, Quote, q, i)
        closes[i] = q->close;
      _EACH
      double *maxs = ATOMIC(sels_size * sizeof(double));
      EACH_IX(sels, Quote, q, i)
        maxs[i] = q->max;
      _EACH
      double *mins = ATOMIC(sels_size * sizeof(double));
      EACH_IX(sels, Quote, q, i)
        mins[i] = q->min;
      _EACH
      int *vols = ATOMIC(sels_size * sizeof(int));
      EACH_IX(sels, Quote, q, i)
        vols[i] = q->vol;
      _EACH
      arr_push(r, quote_new(
        max_date,
        max_double(opens, best_q->open),
        max_double(closes, best_q->close),
        max_double(maxs, best_q->max),
        max_double(mins, best_q->min),
        max_int(vols, best_q->vol),
        0
      ));
    }
  }

  return r;
}

// Returns Kv[Quote]
Kv *quote_corr1 (Quote *q) {
  char *msg = "";
  double open = q->open;
  double close = q->close;
  double max = q->max;
  double min = q->min;
  int vol = q->vol;
  int error = q->error;

  if (!error) {
    if (open > max) {
      max = open;
      error = 1;
      msg = "Open > Max";
    }
    if (close > max) {
      max = close;
      error = 1;
      msg = "Close > Max";
    }
    if (open < min) {
      min = open;
      error = 1;
      msg = "Open < Min";
    }
    if (close < min) {
      min = close;
      error = 1;
      msg = "Close < Min";
    }
  }

  return kv_new(msg, quote_new(q->date, open, close, max, min, vol, error));
}

// Returns Kv[Quote]
Kv *quote_corr2 (Quote *last, Quote *previous) {
  char *msg = "";
  double open = last->open;
  double close = last->close;
  double max = last->max;
  double min = last->min;
  int vol = last->vol;
  int error = last->error;
  double open0 = previous->open;
  double close0 = previous->close;
  double max0 = previous->max;
  double min0 = previous->min;

  if (!error && open0 > 0) {
    if (max0 < min0) return quote_corr1(last);

    if (open > max) {
      if (open == open0 && max != max0) open = max; else max = open;
      error = 1;
      msg = "Open > Max";
    }
    if (close > max) {
      if (close == close0 && max != max0) close = max; else max = close;
      error = 1;
      msg = "Close > Max";
    }
    if (open < min) {
      if (open == open0 && min != min0) open = min; else min = open;
      error = 1;
      msg = "Open < Min";
    }
    if (close < min) {
      if (close == close0 && min != min0) close = min; else min = close;
      error = 1;
      msg = "Close < Min";
    }
  } else if (!error) {
    quote_corr1(last);
  }

  return kv_new(msg, quote_new(last->date, open, close, max, min, vol, error));
}

// Returns Kv[Quote]
Kv *quote_corr3 (Quote *last, Quote *previous) {
  char *msg = "";
  double open = last->open;
  double close = last->close;
  double max = last->max;
  double min = last->min;
  int vol = last->vol;
  int error = last->error;
  double open0 = previous->open;
  double close0 = previous->close;
  double max0 = previous->max;
  double min0 = previous->min;

  if (!error && open0 > 0) {
    if (open > open0 * 1.2) {
      error = 1;
      msg = "Open +20%";
    }
    if (close > close0 * 1.2) {
      error = 1;
      msg = "Close +20%";
    }
    if (max > max0 * 1.2) {
      error = 1;
      msg = "Max +20%";
    }
    if (min > min0 * 1.2) {
      error = 1;
      msg = "Min +20%";
    }

    if (open < open0 * 0.8) {
      error = 1;
      msg = "Open -20%";
    }
    if (close < close0 * 0.8) {
      error = 1;
      msg = "Close -20%";
    }
    if (max < max0 * 0.8) {
      error = 1;
      msg = "Max -20%";
    }
    if (min < min0 * 0.8) {
      error = 1;
      msg = "Min -20%";
    }
  }

  return kv_new(msg, quote_new(last->date, open, close, max, min, vol, error));
}

// Tp[Arr[char], Arr[Quote]]. 'qs' is Arr[Quote]
Tp *quote_check (Arr *qs) {
  // Arr[char]
  Arr *errs = arr_new();
  int qs_size1 = arr_size(qs) - 1;
  int i = 0;
  for (; i < qs_size1; ++i) {
    Quote *q = arr_get(qs, i);
    // Kv[Quote]
    Kv *r = quote_corr2(q, arr_get(qs, i + 1));
    if (*kv_key(r)) {
      arr_push(errs, str_f("%s: %s", q->date, kv_key(r)));
      arr_set(qs, i, kv_value(r));
    }
  }
  Quote *q = arr_get(qs, i);
  // Kv[Quote]
  Kv *r = quote_corr1(q);
  if (*kv_key(r)) {
    arr_push(errs, str_f("%s: %s", q->date, kv_key(r)));
    arr_set(qs, i, kv_value(r));
  }

  i = 0;
  for (; i < qs_size1; ++i) {
    Quote *q = arr_get(qs, i);
    // Kv[Quote]
    Kv *r = quote_corr3(q, arr_get(qs, i + 1));
    if (*kv_key(r)) {
      arr_push(errs, str_f("%s: %s", q->date, kv_key(r)));
      arr_set(qs, i, kv_value(r));
    }
  }

  arr_sort(errs, (FCMP)str_greater);
  arr_reverse(errs);
  return tp_new(errs, qs);
}

// Tp[Arr[char], Arr[Quote]]. 'model' and 'qs' are Arr[Quote]
Tp *quote_check_dates (Arr *model, Arr *qs) {
  // Arr[char]
  Arr *errs = arr_new();
  // Arr[Quote]
  Arr *rqs = arr_new();

  // It[Quote]
  It *model_it = arr_to_it(model);
  // It[Quote]
  It *qs_it = arr_to_it(qs);
  while (it_has_next(model_it) && it_has_next(qs_it)) {
    Quote *mq = it_peek(model_it);
    Quote *qq = it_peek(qs_it);
    int cmp = strcmp(mq->date, qq->date);
    if (cmp > 0) {
      arr_push(rqs, quote_new(mq->date, -1, -1, -1, -1, -1, 1));
      arr_push(errs, str_f("%s: Missing quote", mq->date));
      it_next(model_it);
    } else if (cmp < 0) {
      arr_push(errs, str_f("%s: Extra quote", qq->date));
      it_next(qs_it);
    } else {
      arr_push(rqs, qq);
      it_next(model_it);
      it_next(qs_it);
    }
  }

  if (it_has_next(model_it)) {
    Quote *mq = it_peek(model_it);
    arr_push(errs, str_f("%s: Missing quotes to end" , mq->date));
    while (it_has_next(model_it)) {
      Quote *mq = it_next(model_it);
      arr_push(rqs, quote_new(mq->date, -1, -1, -1, -1, -1, 1));
    }
  }
  // else if (it_has_next(qs_it)) -> Do nothing. It is usual when updating.

  return tp_new(errs, rqs);
}

// Tp[Arr[char], Arr[Quote]]. 'new' and 'old' are Arr[Quote]
Tp *quote_blend (Arr *model, Arr *new, Arr *old) {
  if (!arr_size(new))
    EXC_ILLEGAL_STATE("Empty array");
  if (!arr_size(old))
    EXC_ILLEGAL_STATE("Empty array");


  // Arr[Quote]
  Arr *qs = arr_new();

  // It[Quote]
  It *new_it = arr_to_it(new);
  int fdrop (Quote *q) { return q->open < 0; }
  // It[Quote]
  It *old_it = it_dropf(arr_to_it(old), (FPRED)fdrop);

  Buf *error_dates = buf_new();
  buf_cadd(error_dates,  ' ');
  while (it_has_next(new_it) && it_has_next(old_it)) {
    Quote *nq = it_peek(new_it);
    Quote *oq = it_peek(old_it);
    int cmp = strcmp(nq->date, oq->date);
    if (cmp > 0) {
      arr_push(qs, it_next(new_it));
    } else if (cmp < 0) {
      arr_push(qs, it_next(old_it));
    } else {
      nq = it_next(new_it);
      oq = it_next(old_it);
      if (oq->error) {
        buf_add(error_dates, oq->date);
        buf_cadd(error_dates,  ' ');
      }
      arr_push(qs, oq);
    }
  }
  while (it_has_next(new_it)) {
    arr_push(qs, it_next(new_it));
  }
  while (it_has_next(old_it)) {
    arr_push(qs, it_next(old_it));
  }

  // Arr[char]
  Arr *errs0 = arr_new();
  if (arr_size(model)) {
    // Returns Tp[Arr[char], Arr[Quote]]
    Tp *e_qs = quote_check_dates(model, qs);
    errs0 = tp_e1(e_qs);
    qs = tp_e2(e_qs);
  }

  if (arr_size(qs) > HISTORIC_QUOTES) {
    arr_remove_range(qs, HISTORIC_QUOTES, arr_size(qs));
  }

  // Arr[char]
  Arr *errs = arr_new();

  char *last_date = ((Quote *)arr_get(new, arr_size(new) - 1))->date;
  int qs_size = arr_size(qs);
  char *edates = buf_str(error_dates);
  int i = 0;
  while (1) {
    int ix = i++;
    Quote *q = arr_get(qs, ix);
    if (strcmp(q->date, last_date) < 0) {
      break;
    }
    if (i == qs_size) {
      // Kv[Quote]
      Kv *r = quote_corr1(q);
      char *e = kv_key(r);
      if (*e && str_index(edates, q->date) == -1) {
        arr_push(errs, str_f("%s: %s", q->date, e));
        arr_set(qs, ix, kv_value(r));
      }
      break;
    }
    // Kv[Quote]
    Kv *r = quote_corr2(q, arr_get(qs, ix + 1));
    char *e = kv_key(r);
    if (*e && str_index(edates, q->date) == -1) {
      arr_push(errs, str_f("%s: %s", q->date, e));
      arr_set(qs, ix, kv_value(r));
    }
  }

  i = 0;
  while (1) {
    int ix = i++;
    Quote *q = arr_get(qs, ix);
    if (strcmp(q->date, last_date) < 0 || i == qs_size) {
      break;
    }
    // Kv[Quote]
    Kv *r = quote_corr3(q, arr_get(qs, ix + 1));
    char *e = kv_key(r);
    if (*e && str_index(edates, q->date) == -1) {
      arr_push(errs, str_f("%s: %s", q->date, e));
      arr_set(qs, ix, kv_value(r));
    }
  }

  arr_cat(errs, errs0);
  arr_sort(errs, (FCMP)str_greater);
  arr_reverse(errs);
  return tp_new(errs, qs);
}

