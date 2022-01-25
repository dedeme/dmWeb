// Copyright 16-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[ADouble*].

#ifndef DATA_ADOUBLE_OADOUBLE_H
  #define DATA_ADOUBLE_OADOUBLE_H

#include "dmc/ADouble.h"

/// Opt[ADouble*].
typedef struct oADouble_OADouble OADouble;

/// Returns a none option.
OADouble *oADouble_mk_none();

/// Returns an option with a value.
OADouble *oADouble_mk_some(ADouble *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oADouble_none(OADouble *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
ADouble *oADouble_some(OADouble *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
ADouble *oADouble_esome (OADouble *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
ADouble *oADouble_osome (OADouble *opt, ADouble *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
ADouble *oADouble_nsome (OADouble *opt);

/// Returns this JSONized.
///   this: Container.
char *oADouble_to_js (OADouble *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OADouble *oADouble_from_js (char *js);


//--// Not remove

#endif