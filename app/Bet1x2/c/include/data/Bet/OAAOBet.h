// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[AAOBet*].

#ifndef DATA_BET_OAAOBET_H
  #define DATA_BET_OAAOBET_H

#include "data/Bet/AAOBet.h"

/// Opt[AAOBet*].
typedef struct oAAOBet_OAAOBet OAAOBet;

/// Returns a none option.
OAAOBet *oAAOBet_mk_none();

/// Returns an option with a value.
OAAOBet *oAAOBet_mk_some(AAOBet *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oAAOBet_none(OAAOBet *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
AAOBet *oAAOBet_some(OAAOBet *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
AAOBet *oAAOBet_esome (OAAOBet *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
AAOBet *oAAOBet_osome (OAAOBet *opt, AAOBet *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
AAOBet *oAAOBet_nsome (OAAOBet *opt);

/// Returns this JSONized.
///   this: Container.
char *oAAOBet_to_js (OAAOBet *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OAAOBet *oAAOBet_from_js (char *js);


//--// Not remove

#endif