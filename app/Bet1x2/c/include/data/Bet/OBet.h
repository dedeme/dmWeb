// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[Bet*].

#ifndef DATA_BET_OBET_H
  #define DATA_BET_OBET_H

#include "data/Bet.h"

/// Opt[Bet*].
typedef struct oBet_OBet OBet;

/// Returns a none option.
OBet *oBet_mk_none();

/// Returns an option with a value.
OBet *oBet_mk_some(Bet *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oBet_none(OBet *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
Bet *oBet_some(OBet *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
Bet *oBet_esome (OBet *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
Bet *oBet_osome (OBet *opt, Bet *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
Bet *oBet_nsome (OBet *opt);

/// Returns this JSONized.
///   this: Container.
char *oBet_to_js (OBet *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OBet *oBet_from_js (char *js);


//--// Not remove

#endif