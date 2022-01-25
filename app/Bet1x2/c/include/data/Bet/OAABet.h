// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[AABet*].

#ifndef DATA_BET_OAABET_H
  #define DATA_BET_OAABET_H

#include "data/Bet/AABet.h"

/// Opt[AABet*].
typedef struct oAABet_OAABet OAABet;

/// Returns a none option.
OAABet *oAABet_mk_none();

/// Returns an option with a value.
OAABet *oAABet_mk_some(AABet *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oAABet_none(OAABet *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
AABet *oAABet_some(OAABet *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
AABet *oAABet_esome (OAABet *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
AABet *oAABet_osome (OAABet *opt, AABet *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
AABet *oAABet_nsome (OAABet *opt);

/// Returns this JSONized.
///   this: Container.
char *oAABet_to_js (OAABet *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OAABet *oAABet_from_js (char *js);


//--// Not remove

#endif