// Copyright 28-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[SimProfitsRow*].

#ifndef DATA_SIMULATION_SIMPROFITSROW_OSIMPROFITSROW_H
  #define DATA_SIMULATION_SIMPROFITSROW_OSIMPROFITSROW_H

#include "data/simulation/SimProfitsRow.h"

/// Opt[SimProfitsRow*].
typedef struct oSimProfitsRow_OSimProfitsRow OSimProfitsRow;

/// Returns a none option.
OSimProfitsRow *oSimProfitsRow_mk_none();

/// Returns an option with a value.
OSimProfitsRow *oSimProfitsRow_mk_some(SimProfitsRow *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oSimProfitsRow_none(OSimProfitsRow *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
SimProfitsRow *oSimProfitsRow_some(OSimProfitsRow *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
SimProfitsRow *oSimProfitsRow_esome (OSimProfitsRow *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
SimProfitsRow *oSimProfitsRow_osome (OSimProfitsRow *opt, SimProfitsRow *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
SimProfitsRow *oSimProfitsRow_nsome (OSimProfitsRow *opt);

/// Returns this JSONized.
///   this: Container.
char *oSimProfitsRow_to_js (OSimProfitsRow *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OSimProfitsRow *oSimProfitsRow_from_js (char *js);


//--// Not remove

#endif