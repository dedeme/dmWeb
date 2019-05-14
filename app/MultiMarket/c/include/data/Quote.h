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

#endif
