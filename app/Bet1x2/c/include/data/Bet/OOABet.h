// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[OABet*].

#ifndef DATA_BET_OOABET_H
  #define DATA_BET_OOABET_H

#include "data/Bet/OABet.h"

/// Opt[OABet*].
typedef struct oOABet_OOABet OOABet;

/// Returns a none option.
OOABet *oOABet_mk_none();

/// Returns an option with a value.
OOABet *oOABet_mk_some(OABet *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oOABet_none(OOABet *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
OABet *oOABet_some(OOABet *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
OABet *oOABet_esome (OOABet *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
OABet *oOABet_osome (OOABet *opt, OABet *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
OABet *oOABet_nsome (OOABet *opt);

/// Returns this JSONized.
///   this: Container.
char *oOABet_to_js (OOABet *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OOABet *oOABet_from_js (char *js);


//--// Not remove

#endif