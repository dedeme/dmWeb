// Copyright 07-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_QUOTE_H
  #define DATA_QUOTE_H

#include "dmc/async.h"

/// 'quote_test' or 'quote_test2' is ok.
#define quote_OK "QuoteOk"
/// Open > Max
#define quote_OMAX "QuoteOmax"
/// Close > Max
#define quote_CMAX "QuoteCmax"
/// Open < Min
#define quote_OMIN "QuoteOmin"
/// Close < Min
#define quote_CMIN "QuoteCmin"

/// A date is missing
#define quote_NO_DATE "QuoteNoDate"
/// There is an extra date
#define quote_EXTRA_DATE "QuoteExtraDate"
/// Open is +- 20% of its previous value
#define quote_OPEN20 "QuoteOpen20"
/// Close is +- 20% of its previous value
#define quote_CLOSE20 "QuoteClose20"
/// Maximum is +- 20% of its previous value
#define quote_MAX20 "QuoteMax20"
/// Minimum is +- 20% of its previous value
#define quote_MIN20 "QuoteMin20"

/*--*/

///
typedef struct Quote_Quote Quote;

///
Quote *quote_new(
  char *date,
  double open,
  double close,
  double max,
  double min,
  int vol,
  int error
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

/// It it is 1, quotes are handly modified.
int quote_error(Quote *this);

///
Js *quote_to_js(Quote *this);

///
Quote *quote_from_js(Js *js);

/*--*/

/// Tests if 'q' is correct.
///   return: quote_OK, quote_OMAX, quote_CMAX, quote_OMIN, quote_CMIN,
///           quote_MAX_MIN.
char *quote_test (Quote *this);

/// Tests if 'q' is correct in relation with 'q_model' and 'previous_q'.
///   return: quote_OK, quote_NO_DATE, quote_EXTRA_DATE, quote_OPEN20
///           quote_CLOSE20, quote_MAX20, quote_MIN20
char *quote_test2 (Quote *this, Quote *q_model, Quote *previous_q);

/// Returns Opt[Quote]
Opt *quote_from_str (char *q);

///
char *quote_to_str (Quote *q);

/// Unifies serveral arrays of quotes in one.
///   aqs: Arr[Arr[Quote]]] Arrays to unify
///   qs: Arr[Quote] One of 'aqs' selected for tiebreak
///   return Arr[Quote] A new array with the result.
Arr *quote_unify (Arr *aqs, Arr *best_qs);

/// Checks maximum and minimum and returns a new quote corrected.<p>
/// If quote_error(q) = 1, quote will not be corrected.<p>
/// If quote was corrected, its 'error' field is set to 1
///   q     : Quote to correct
///   return: Kv[Quote]
///           - If there was an error 'kv_key' is a message (e.g. Max > Min)
///             and 'kv_value' is a new quote equals to 'q' corrected.
///           - If there was no error 'kv_key' is a blank string and 'kv_value'
///             is a new quote equals to 'q'.
Kv *quote_corr1 (Quote *q);

/// Checks maximum and minimum and returns a new quote corrected.<p>
/// If quote_error(last) = 1, quote will not be corrected.<p>
/// If quote was corrected, its 'error' field is set to 1
///   last: Quote to correct
///   previous: Quote previous to 'last'
///   return: Kv[Quote]
///           - If there was an error 'kv_key' is a message (e.g. Max > Min)
///             and 'kv_value' is a new quote equals to 'q' corrected.
///           - If there was no error 'kv_key' is a blank string and 'kv_value'
///             is a new quote equals to 'q'.
Kv *quote_corr2 (Quote *last, Quote *previous);

/// Checks increment and returns a new quote corrected.<p>
/// If quote_error(last) = 1, quote will not be corrected.<p>
/// If quote has an incerment +-20%, its 'error' field is set to 1
///   last: Quote to correct
///   previous: Quote previous to 'last'
///   return: Kv[Quote]
///           - If there was an error 'kv_key' is a message (e.g. Max > Min)
///             and 'kv_value' is a new quote equals to 'q' corrected.
///           - If there was no error 'kv_key' is a blank string and 'kv_value'
///             is a new quote equals to 'q'.
Kv *quote_corr3 (Quote *last, Quote *previous);

/// Checks quotes which field 'error' is '= 0', in 'qs'.
///   qs: Arr[Quote] Quotes to check.
///   return: Tp of
///     Arr[char] Errors returned by 'corr1', 'corr2' and 'corr3' with format
///       "date: error". If there is no error, the array is empty.
///     Arr[Quote]. Array corrected.
Tp *quote_check (Arr *qs);

/// Checks dates of 'qs' matching them with the ones of 'model'.
///   model: Arr[Quote] Quotes of nick model.
///   qs: Arr[Quote] Quotes to check.
///   return: Tp of
///     Arr[char] Errors by extra or missing quotes. If there is no error,
///       the array is empty.
///     Arr[Quote]. Array corrected.
Tp *quote_check_dates (Arr *model, Arr *qs);

/// Blends new quotes with others already existent.<p>
/// All Arr are Arr[Quote]
///   model : Arr[Quote]. Model quotes. It can be empty.
///   new   : Arr[Quote]. Last quotes read from the Internet. It can be empty.
///   old   : Arr[Quote]. Existent quotes in file system. It can be empty.
///   return: Tp of
///     Arr[char] Errors returned by 'corr1' and 'corr2' with format
///       "date: error". If there is no error, the array is empty.
///     Arr[Quote]. Array made with the following process:
///        1. Every quote on top with 'open = -1' is removed from 'old' in the
///           dates range of 'new'
///        2. If there are new and old quotes for the same date, that of 'old'
///           is selected.
///        3. The return array is corrected in the range of 'new' dates and
///           adding or removing quotes maching model quotes.
Tp *quote_blend (Arr *model, Arr *new, Arr *old);

#endif