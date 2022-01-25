// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[AOBet*].

#ifndef DATA_BET_OAOBET_H
  #define DATA_BET_OAOBET_H

#include "data/Bet/AOBet.h"

/// Opt[AOBet*].
typedef struct oAOBet_OAOBet OAOBet;

/// Returns a none option.
OAOBet *oAOBet_mk_none();

/// Returns an option with a value.
OAOBet *oAOBet_mk_some(AOBet *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oAOBet_none(OAOBet *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
AOBet *oAOBet_some(OAOBet *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
AOBet *oAOBet_esome (OAOBet *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
AOBet *oAOBet_osome (OAOBet *opt, AOBet *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
AOBet *oAOBet_nsome (OAOBet *opt);

/// Returns this JSONized.
///   this: Container.
char *oAOBet_to_js (OAOBet *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OAOBet *oAOBet_from_js (char *js);


//--// Not remove

#endif