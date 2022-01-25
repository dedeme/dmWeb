// Copyright 28-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[Decision*].

#ifndef DATA_DECISION_ODECISION_H
  #define DATA_DECISION_ODECISION_H

#include "data/Decision.h"

/// Opt[Decision*].
typedef struct oDecision_ODecision ODecision;

/// Returns a none option.
ODecision *oDecision_mk_none();

/// Returns an option with a value.
ODecision *oDecision_mk_some(Decision *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oDecision_none(ODecision *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
Decision *oDecision_some(ODecision *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
Decision *oDecision_esome (ODecision *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
Decision *oDecision_osome (ODecision *opt, Decision *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
Decision *oDecision_nsome (ODecision *opt);

/// Returns this JSONized.
///   this: Container.
char *oDecision_to_js (ODecision *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
ODecision *oDecision_from_js (char *js);


//--// Not remove

#endif