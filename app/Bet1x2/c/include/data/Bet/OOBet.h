// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[OBet*].

#ifndef DATA_BET_OOBET_H
  #define DATA_BET_OOBET_H

#include "data/Bet/OBet.h"

/// Opt[OBet*].
typedef struct oOBet_OOBet OOBet;

/// Returns a none option.
OOBet *oOBet_mk_none();

/// Returns an option with a value.
OOBet *oOBet_mk_some(OBet *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oOBet_none(OOBet *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
OBet *oOBet_some(OOBet *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
OBet *oOBet_esome (OOBet *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
OBet *oOBet_osome (OOBet *opt, OBet *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
OBet *oOBet_nsome (OOBet *opt);

/// Returns this JSONized.
///   this: Container.
char *oOBet_to_js (OOBet *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OOBet *oOBet_from_js (char *js);


//--// Not remove

#endif