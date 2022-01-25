// Copyright 28-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[ODecision*].

#ifndef DATA_DECISION_OODECISION_H
  #define DATA_DECISION_OODECISION_H

#include "data/Decision/ODecision.h"

/// Opt[ODecision*].
typedef struct oODecision_OODecision OODecision;

/// Returns a none option.
OODecision *oODecision_mk_none();

/// Returns an option with a value.
OODecision *oODecision_mk_some(ODecision *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oODecision_none(OODecision *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
ODecision *oODecision_some(OODecision *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
ODecision *oODecision_esome (OODecision *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
ODecision *oODecision_osome (OODecision *opt, ODecision *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
ODecision *oODecision_nsome (OODecision *opt);

/// Returns this JSONized.
///   this: Container.
char *oODecision_to_js (OODecision *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OODecision *oODecision_from_js (char *js);


//--// Not remove

#endif