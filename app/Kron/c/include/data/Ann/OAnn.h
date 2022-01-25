// Copyright 25-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[Ann*].

#ifndef DATA_ANN_OANN_H
  #define DATA_ANN_OANN_H

#include "data/Ann.h"

/// Opt[Ann*].
typedef struct oAnn_OAnn OAnn;

/// Returns a none option.
OAnn *oAnn_mk_none();

/// Returns an option with a value.
OAnn *oAnn_mk_some(Ann *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oAnn_none(OAnn *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
Ann *oAnn_some(OAnn *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
Ann *oAnn_esome (OAnn *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
Ann *oAnn_osome (OAnn *opt, Ann *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
Ann *oAnn_nsome (OAnn *opt);

/// Returns this JSONized.
///   this: Container.
char *oAnn_to_js (OAnn *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OAnn *oAnn_from_js (char *js);


//--// Not remove

#endif