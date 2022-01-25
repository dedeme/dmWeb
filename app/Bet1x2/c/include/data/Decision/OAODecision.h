// Copyright 28-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[AODecision*].

#ifndef DATA_DECISION_OAODECISION_H
  #define DATA_DECISION_OAODECISION_H

#include "data/Decision/AODecision.h"

/// Opt[AODecision*].
typedef struct oAODecision_OAODecision OAODecision;

/// Returns a none option.
OAODecision *oAODecision_mk_none();

/// Returns an option with a value.
OAODecision *oAODecision_mk_some(AODecision *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oAODecision_none(OAODecision *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
AODecision *oAODecision_some(OAODecision *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
AODecision *oAODecision_esome (OAODecision *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
AODecision *oAODecision_osome (OAODecision *opt, AODecision *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
AODecision *oAODecision_nsome (OAODecision *opt);

/// Returns this JSONized.
///   this: Container.
char *oAODecision_to_js (OAODecision *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OAODecision *oAODecision_from_js (char *js);


//--// Not remove

#endif