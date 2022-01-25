// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[OOOResult*].

#ifndef DATA_RESULT_OOOORESULT_H
  #define DATA_RESULT_OOOORESULT_H

#include "data/Result/OOOResult.h"

/// Opt[OOOResult*].
typedef struct oOOOResult_OOOOResult OOOOResult;

/// Returns a none option.
OOOOResult *oOOOResult_mk_none();

/// Returns an option with a value.
OOOOResult *oOOOResult_mk_some(OOOResult *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oOOOResult_none(OOOOResult *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
OOOResult *oOOOResult_some(OOOOResult *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
OOOResult *oOOOResult_esome (OOOOResult *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
OOOResult *oOOOResult_osome (OOOOResult *opt, OOOResult *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
OOOResult *oOOOResult_nsome (OOOOResult *opt);

/// Returns this JSONized.
///   this: Container.
char *oOOOResult_to_js (OOOOResult *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OOOOResult *oOOOResult_from_js (char *js);


//--// Not remove

#endif