// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[OOResult*].

#ifndef DATA_RESULT_OOORESULT_H
  #define DATA_RESULT_OOORESULT_H

#include "data/Result/OOResult.h"

/// Opt[OOResult*].
typedef struct oOOResult_OOOResult OOOResult;

/// Returns a none option.
OOOResult *oOOResult_mk_none();

/// Returns an option with a value.
OOOResult *oOOResult_mk_some(OOResult *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oOOResult_none(OOOResult *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
OOResult *oOOResult_some(OOOResult *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
OOResult *oOOResult_esome (OOOResult *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
OOResult *oOOResult_osome (OOOResult *opt, OOResult *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
OOResult *oOOResult_nsome (OOOResult *opt);

/// Returns this JSONized.
///   this: Container.
char *oOOResult_to_js (OOOResult *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OOOResult *oOOResult_from_js (char *js);


//--// Not remove

#endif