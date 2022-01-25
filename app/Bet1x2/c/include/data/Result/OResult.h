// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[Result*].

#ifndef DATA_RESULT_ORESULT_H
  #define DATA_RESULT_ORESULT_H

#include "data/Result.h"

/// Opt[Result*].
typedef struct oResult_OResult OResult;

/// Returns a none option.
OResult *oResult_mk_none();

/// Returns an option with a value.
OResult *oResult_mk_some(Result *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oResult_none(OResult *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
Result *oResult_some(OResult *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
Result *oResult_esome (OResult *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
Result *oResult_osome (OResult *opt, Result *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
Result *oResult_nsome (OResult *opt);

/// Returns this JSONized.
///   this: Container.
char *oResult_to_js (OResult *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OResult *oResult_from_js (char *js);


//--// Not remove

int oResult_eq (OResult *this, OResult *other);

#endif
