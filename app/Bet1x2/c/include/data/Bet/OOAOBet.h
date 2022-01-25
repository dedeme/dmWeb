// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[OAOBet*].

#ifndef DATA_BET_OOAOBET_H
  #define DATA_BET_OOAOBET_H

#include "data/Bet/OAOBet.h"

/// Opt[OAOBet*].
typedef struct oOAOBet_OOAOBet OOAOBet;

/// Returns a none option.
OOAOBet *oOAOBet_mk_none();

/// Returns an option with a value.
OOAOBet *oOAOBet_mk_some(OAOBet *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oOAOBet_none(OOAOBet *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
OAOBet *oOAOBet_some(OOAOBet *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
OAOBet *oOAOBet_esome (OOAOBet *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
OAOBet *oOAOBet_osome (OOAOBet *opt, OAOBet *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
OAOBet *oOAOBet_nsome (OOAOBet *opt);

/// Returns this JSONized.
///   this: Container.
char *oOAOBet_to_js (OOAOBet *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OOAOBet *oOAOBet_from_js (char *js);


//--// Not remove

#endif