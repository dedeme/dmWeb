// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[OOBet*].

#ifndef DATA_BET_OOOBET_H
  #define DATA_BET_OOOBET_H

#include "data/Bet/OOBet.h"

/// Opt[OOBet*].
typedef struct oOOBet_OOOBet OOOBet;

/// Returns a none option.
OOOBet *oOOBet_mk_none();

/// Returns an option with a value.
OOOBet *oOOBet_mk_some(OOBet *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oOOBet_none(OOOBet *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
OOBet *oOOBet_some(OOOBet *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
OOBet *oOOBet_esome (OOOBet *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
OOBet *oOOBet_osome (OOOBet *opt, OOBet *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
OOBet *oOOBet_nsome (OOOBet *opt);

/// Returns this JSONized.
///   this: Container.
char *oOOBet_to_js (OOOBet *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OOOBet *oOOBet_from_js (char *js);


//--// Not remove

#endif