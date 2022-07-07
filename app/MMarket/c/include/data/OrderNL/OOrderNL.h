// Copyright 29-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[OrderNL*].

#ifndef DATA_ORDERNL_OORDERNL_H
  #define DATA_ORDERNL_OORDERNL_H

#include "data/OrderNL.h"

/// Opt[OrderNL*].
typedef struct oOrderNL_OOrderNL OOrderNL;

/// Returns a none option.
OOrderNL *oOrderNL_mk_none();

/// Returns an option with a value.
OOrderNL *oOrderNL_mk_some(OrderNL *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oOrderNL_none(OOrderNL *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
OrderNL *oOrderNL_some(OOrderNL *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
OrderNL *oOrderNL_esome (OOrderNL *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
OrderNL *oOrderNL_osome (OOrderNL *opt, OrderNL *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
OrderNL *oOrderNL_nsome (OOrderNL *opt);

/// Returns this JSONized.
///   this: Container.
char *oOrderNL_to_js (OOrderNL *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OOrderNL *oOrderNL_from_js (char *js);


//--// Not remove

#endif