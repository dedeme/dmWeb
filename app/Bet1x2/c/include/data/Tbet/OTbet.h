// Copyright 25-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[Tbet*].

#ifndef DATA_TBET_OTBET_H
  #define DATA_TBET_OTBET_H

#include "data/Tbet.h"

/// Opt[Tbet*].
typedef struct oTbet_OTbet OTbet;

/// Returns a none option.
OTbet *oTbet_mk_none();

/// Returns an option with a value.
OTbet *oTbet_mk_some(Tbet *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oTbet_none(OTbet *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
Tbet *oTbet_some(OTbet *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
Tbet *oTbet_esome (OTbet *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
Tbet *oTbet_osome (OTbet *opt, Tbet *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
Tbet *oTbet_nsome (OTbet *opt);

/// Returns this JSONized.
///   this: Container.
char *oTbet_to_js (OTbet *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OTbet *oTbet_from_js (char *js);


//--// Not remove

#endif