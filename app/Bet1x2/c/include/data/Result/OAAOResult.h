// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[AAOResult*].

#ifndef DATA_RESULT_OAAORESULT_H
  #define DATA_RESULT_OAAORESULT_H

#include "data/Result/AAOResult.h"

/// Opt[AAOResult*].
typedef struct oAAOResult_OAAOResult OAAOResult;

/// Returns a none option.
OAAOResult *oAAOResult_mk_none();

/// Returns an option with a value.
OAAOResult *oAAOResult_mk_some(AAOResult *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oAAOResult_none(OAAOResult *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
AAOResult *oAAOResult_some(OAAOResult *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
AAOResult *oAAOResult_esome (OAAOResult *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
AAOResult *oAAOResult_osome (OAAOResult *opt, AAOResult *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
AAOResult *oAAOResult_nsome (OAAOResult *opt);

/// Returns this JSONized.
///   this: Container.
char *oAAOResult_to_js (OAAOResult *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OAAOResult *oAAOResult_from_js (char *js);


//--// Not remove

#endif