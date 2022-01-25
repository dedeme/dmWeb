// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[AResult*].

#ifndef DATA_RESULT_OARESULT_H
  #define DATA_RESULT_OARESULT_H

#include "data/Result/AResult.h"

/// Opt[AResult*].
typedef struct oAResult_OAResult OAResult;

/// Returns a none option.
OAResult *oAResult_mk_none();

/// Returns an option with a value.
OAResult *oAResult_mk_some(AResult *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oAResult_none(OAResult *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
AResult *oAResult_some(OAResult *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
AResult *oAResult_esome (OAResult *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
AResult *oAResult_osome (OAResult *opt, AResult *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
AResult *oAResult_nsome (OAResult *opt);

/// Returns this JSONized.
///   this: Container.
char *oAResult_to_js (OAResult *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OAResult *oAResult_from_js (char *js);


//--// Not remove

#endif