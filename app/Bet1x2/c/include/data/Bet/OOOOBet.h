// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[OOOBet*].

#ifndef DATA_BET_OOOOBET_H
  #define DATA_BET_OOOOBET_H

#include "data/Bet/OOOBet.h"

/// Opt[OOOBet*].
typedef struct oOOOBet_OOOOBet OOOOBet;

/// Returns a none option.
OOOOBet *oOOOBet_mk_none();

/// Returns an option with a value.
OOOOBet *oOOOBet_mk_some(OOOBet *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oOOOBet_none(OOOOBet *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
OOOBet *oOOOBet_some(OOOOBet *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
OOOBet *oOOOBet_esome (OOOOBet *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
OOOBet *oOOOBet_osome (OOOOBet *opt, OOOBet *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
OOOBet *oOOOBet_nsome (OOOOBet *opt);

/// Returns this JSONized.
///   this: Container.
char *oOOOBet_to_js (OOOOBet *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OOOOBet *oOOOBet_from_js (char *js);


//--// Not remove

#endif