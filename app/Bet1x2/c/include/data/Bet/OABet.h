// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[ABet*].

#ifndef DATA_BET_OABET_H
  #define DATA_BET_OABET_H

#include "data/Bet/ABet.h"

/// Opt[ABet*].
typedef struct oABet_OABet OABet;

/// Returns a none option.
OABet *oABet_mk_none();

/// Returns an option with a value.
OABet *oABet_mk_some(ABet *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oABet_none(OABet *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
ABet *oABet_some(OABet *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
ABet *oABet_esome (OABet *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
ABet *oABet_osome (OABet *opt, ABet *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
ABet *oABet_nsome (OABet *opt);

/// Returns this JSONized.
///   this: Container.
char *oABet_to_js (OABet *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OABet *oABet_from_js (char *js);


//--// Not remove

#endif