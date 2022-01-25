// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[AOResult*].

#ifndef DATA_RESULT_OAORESULT_H
  #define DATA_RESULT_OAORESULT_H

#include "data/Result/AOResult.h"

/// Opt[AOResult*].
typedef struct oAOResult_OAOResult OAOResult;

/// Returns a none option.
OAOResult *oAOResult_mk_none();

/// Returns an option with a value.
OAOResult *oAOResult_mk_some(AOResult *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oAOResult_none(OAOResult *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
AOResult *oAOResult_some(OAOResult *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
AOResult *oAOResult_esome (OAOResult *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
AOResult *oAOResult_osome (OAOResult *opt, AOResult *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
AOResult *oAOResult_nsome (OAOResult *opt);

/// Returns this JSONized.
///   this: Container.
char *oAOResult_to_js (OAOResult *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OAOResult *oAOResult_from_js (char *js);


//--// Not remove

#endif