// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[AAResult*].

#ifndef DATA_RESULT_OAARESULT_H
  #define DATA_RESULT_OAARESULT_H

#include "data/Result/AAResult.h"

/// Opt[AAResult*].
typedef struct oAAResult_OAAResult OAAResult;

/// Returns a none option.
OAAResult *oAAResult_mk_none();

/// Returns an option with a value.
OAAResult *oAAResult_mk_some(AAResult *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oAAResult_none(OAAResult *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
AAResult *oAAResult_some(OAAResult *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
AAResult *oAAResult_esome (OAAResult *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
AAResult *oAAResult_osome (OAAResult *opt, AAResult *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
AAResult *oAAResult_nsome (OAAResult *opt);

/// Returns this JSONized.
///   this: Container.
char *oAAResult_to_js (OAAResult *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OAAResult *oAAResult_from_js (char *js);


//--// Not remove

#endif