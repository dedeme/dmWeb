// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[OAOResult*].

#ifndef DATA_RESULT_OOAORESULT_H
  #define DATA_RESULT_OOAORESULT_H

#include "data/Result/OAOResult.h"

/// Opt[OAOResult*].
typedef struct oOAOResult_OOAOResult OOAOResult;

/// Returns a none option.
OOAOResult *oOAOResult_mk_none();

/// Returns an option with a value.
OOAOResult *oOAOResult_mk_some(OAOResult *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oOAOResult_none(OOAOResult *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
OAOResult *oOAOResult_some(OOAOResult *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
OAOResult *oOAOResult_esome (OOAOResult *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
OAOResult *oOAOResult_osome (OOAOResult *opt, OAOResult *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
OAOResult *oOAOResult_nsome (OOAOResult *opt);

/// Returns this JSONized.
///   this: Container.
char *oOAOResult_to_js (OOAOResult *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OOAOResult *oOAOResult_from_js (char *js);


//--// Not remove

#endif