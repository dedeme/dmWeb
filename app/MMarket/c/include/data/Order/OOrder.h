// Copyright 22-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[Order*].

#ifndef DATA_ORDER_OORDER_H
  #define DATA_ORDER_OORDER_H

#include "data/Order.h"

/// Opt[Order*].
typedef struct oOrder_OOrder OOrder;

/// Returns a none option.
OOrder *oOrder_mk_none();

/// Returns an option with a value.
OOrder *oOrder_mk_some(Order *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oOrder_none(OOrder *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
Order *oOrder_some(OOrder *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
Order *oOrder_esome (OOrder *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
Order *oOrder_osome (OOrder *opt, Order *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
Order *oOrder_nsome (OOrder *opt);

/// Returns this JSONized.
///   this: Container.
char *oOrder_to_js (OOrder *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OOrder *oOrder_from_js (char *js);


//--// Not remove

#endif