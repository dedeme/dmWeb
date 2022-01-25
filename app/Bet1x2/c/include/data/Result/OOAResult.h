// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[OAResult*].

#ifndef DATA_RESULT_OOARESULT_H
  #define DATA_RESULT_OOARESULT_H

#include "data/Result/OAResult.h"

/// Opt[OAResult*].
typedef struct oOAResult_OOAResult OOAResult;

/// Returns a none option.
OOAResult *oOAResult_mk_none();

/// Returns an option with a value.
OOAResult *oOAResult_mk_some(OAResult *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oOAResult_none(OOAResult *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
OAResult *oOAResult_some(OOAResult *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
OAResult *oOAResult_esome (OOAResult *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
OAResult *oOAResult_osome (OOAResult *opt, OAResult *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
OAResult *oOAResult_nsome (OOAResult *opt);

/// Returns this JSONized.
///   this: Container.
char *oOAResult_to_js (OOAResult *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OOAResult *oOAResult_from_js (char *js);


//--// Not remove

#endif