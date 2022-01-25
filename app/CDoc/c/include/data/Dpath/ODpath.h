// Copyright 25-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[Dpath*].

#ifndef DATA_DPATH_ODPATH_H
  #define DATA_DPATH_ODPATH_H

#include "data/Dpath.h"

/// Opt[Dpath*].
typedef struct oDpath_ODpath ODpath;

/// Returns a none option.
ODpath *oDpath_mk_none();

/// Returns an option with a value.
ODpath *oDpath_mk_some(Dpath *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oDpath_none(ODpath *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
Dpath *oDpath_some(ODpath *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
Dpath *oDpath_esome (ODpath *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
Dpath *oDpath_osome (ODpath *opt, Dpath *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
Dpath *oDpath_nsome (ODpath *opt);

/// Returns this JSONized.
///   this: Container.
char *oDpath_to_js (ODpath *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
ODpath *oDpath_from_js (char *js);


//--// Not remove

#endif