// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[Profits*].

#ifndef DATA_PROFITS_OPROFITS_H
  #define DATA_PROFITS_OPROFITS_H

#include "data/Profits.h"

/// Opt[Profits*].
typedef struct oProfits_OProfits OProfits;

/// Returns a none option.
OProfits *oProfits_mk_none();

/// Returns an option with a value.
OProfits *oProfits_mk_some(Profits *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oProfits_none(OProfits *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
Profits *oProfits_some(OProfits *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
Profits *oProfits_esome (OProfits *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
Profits *oProfits_osome (OProfits *opt, Profits *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
Profits *oProfits_nsome (OProfits *opt);

/// Returns this JSONized.
///   this: Container.
char *oProfits_to_js (OProfits *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OProfits *oProfits_from_js (char *js);


//--// Not remove

#endif