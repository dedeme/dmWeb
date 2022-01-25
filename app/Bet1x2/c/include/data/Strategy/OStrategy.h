// Copyright 28-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[Strategy*].

#ifndef DATA_STRATEGY_OSTRATEGY_H
  #define DATA_STRATEGY_OSTRATEGY_H

#include "data/Strategy.h"

/// Opt[Strategy*].
typedef struct oStrategy_OStrategy OStrategy;

/// Returns a none option.
OStrategy *oStrategy_mk_none();

/// Returns an option with a value.
OStrategy *oStrategy_mk_some(Strategy *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oStrategy_none(OStrategy *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
Strategy *oStrategy_some(OStrategy *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
Strategy *oStrategy_esome (OStrategy *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
Strategy *oStrategy_osome (OStrategy *opt, Strategy *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
Strategy *oStrategy_nsome (OStrategy *opt);

/// Returns this JSONized.
///   this: Container.
char *oStrategy_to_js (OStrategy *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OStrategy *oStrategy_from_js (char *js);


//--// Not remove

#endif