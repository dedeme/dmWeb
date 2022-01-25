// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[OResult*].

#ifndef DATA_RESULT_OORESULT_H
  #define DATA_RESULT_OORESULT_H

#include "data/Result/OResult.h"

/// Opt[OResult*].
typedef struct oOResult_OOResult OOResult;

/// Returns a none option.
OOResult *oOResult_mk_none();

/// Returns an option with a value.
OOResult *oOResult_mk_some(OResult *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oOResult_none(OOResult *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
OResult *oOResult_some(OOResult *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
OResult *oOResult_esome (OOResult *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
OResult *oOResult_osome (OOResult *opt, OResult *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
OResult *oOResult_nsome (OOResult *opt);

/// Returns this JSONized.
///   this: Container.
char *oOResult_to_js (OOResult *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OOResult *oOResult_from_js (char *js);


//--// Not remove

#endif