// Copyright 28-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[ADecision*].

#ifndef DATA_DECISION_OADECISION_H
  #define DATA_DECISION_OADECISION_H

#include "data/Decision/ADecision.h"

/// Opt[ADecision*].
typedef struct oADecision_OADecision OADecision;

/// Returns a none option.
OADecision *oADecision_mk_none();

/// Returns an option with a value.
OADecision *oADecision_mk_some(ADecision *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oADecision_none(OADecision *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
ADecision *oADecision_some(OADecision *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
ADecision *oADecision_esome (OADecision *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
ADecision *oADecision_osome (OADecision *opt, ADecision *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
ADecision *oADecision_nsome (OADecision *opt);

/// Returns this JSONized.
///   this: Container.
char *oADecision_to_js (OADecision *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OADecision *oADecision_from_js (char *js);


//--// Not remove

#endif