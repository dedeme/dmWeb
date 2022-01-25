// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[AOOResult*].

#ifndef DATA_RESULT_OAOORESULT_H
  #define DATA_RESULT_OAOORESULT_H

#include "data/Result/AOOResult.h"

/// Opt[AOOResult*].
typedef struct oAOOResult_OAOOResult OAOOResult;

/// Returns a none option.
OAOOResult *oAOOResult_mk_none();

/// Returns an option with a value.
OAOOResult *oAOOResult_mk_some(AOOResult *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oAOOResult_none(OAOOResult *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
AOOResult *oAOOResult_some(OAOOResult *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
AOOResult *oAOOResult_esome (OAOOResult *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
AOOResult *oAOOResult_osome (OAOOResult *opt, AOOResult *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
AOOResult *oAOOResult_nsome (OAOOResult *opt);

/// Returns this JSONized.
///   this: Container.
char *oAOOResult_to_js (OAOOResult *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OAOOResult *oAOOResult_from_js (char *js);


//--// Not remove

#endif