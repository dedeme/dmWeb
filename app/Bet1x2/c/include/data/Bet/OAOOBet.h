// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[AOOBet*].

#ifndef DATA_BET_OAOOBET_H
  #define DATA_BET_OAOOBET_H

#include "data/Bet/AOOBet.h"

/// Opt[AOOBet*].
typedef struct oAOOBet_OAOOBet OAOOBet;

/// Returns a none option.
OAOOBet *oAOOBet_mk_none();

/// Returns an option with a value.
OAOOBet *oAOOBet_mk_some(AOOBet *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oAOOBet_none(OAOOBet *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
AOOBet *oAOOBet_some(OAOOBet *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
AOOBet *oAOOBet_esome (OAOOBet *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
AOOBet *oAOOBet_osome (OAOOBet *opt, AOOBet *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
AOOBet *oAOOBet_nsome (OAOOBet *opt);

/// Returns this JSONized.
///   this: Container.
char *oAOOBet_to_js (OAOOBet *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OAOOBet *oAOOBet_from_js (char *js);


//--// Not remove

#endif