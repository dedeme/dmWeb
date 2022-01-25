// Copyright 28-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[OODecision*].

#ifndef DATA_DECISION_OOODECISION_H
  #define DATA_DECISION_OOODECISION_H

#include "data/Decision/OODecision.h"

/// Opt[OODecision*].
typedef struct oOODecision_OOODecision OOODecision;

/// Returns a none option.
OOODecision *oOODecision_mk_none();

/// Returns an option with a value.
OOODecision *oOODecision_mk_some(OODecision *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oOODecision_none(OOODecision *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
OODecision *oOODecision_some(OOODecision *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
OODecision *oOODecision_esome (OOODecision *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
OODecision *oOODecision_osome (OOODecision *opt, OODecision *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
OODecision *oOODecision_nsome (OOODecision *opt);

/// Returns this JSONized.
///   this: Container.
char *oOODecision_to_js (OOODecision *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OOODecision *oOODecision_from_js (char *js);


//--// Not remove

#endif