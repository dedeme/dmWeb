// Copyright 11-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Broker.

#ifndef DATA_FLEA_H
  #define DATA_FLEA_H

#include "dmc/std.h"
#include "Gen.h"

/*--*/

///
typedef struct Flea_Flea Flea;

///
char *flea_date(Flea *this);

///
int flea_cycle(Flea *this);

///
int flea_id(Flea *this);

///
Gen *flea_gen(Flea *this);

///
Js *flea_to_js(Flea *this);

///
Flea *flea_from_js(Js *js);

/*--*/

#endif

/// Creates a new flea.
///   date: Date of creation.
///   cycle: Cycle of creation.
///   id: Identifier number in cycle.
///   n: Number of elements of its gen.
Flea *flea_new (char *date, int cycle, int id, int n);

/// Returns a new Flea mutation of 'this', with a new identifier 'id'.
///   this: Flea
///   date: Date of creation.
///   cycle: Cycle of creation.
///   id: Identifier number in cycle.
Flea *flea_mutate (Flea *this, char *date, int cycle, int id);

/// Returns date-cycle-id
char *flea_name (Flea *this);

/// Calculates value of a gen parameter.
///   mx: Maximun value of parameter
///   mn: Minimum value of parameter
///   value: Gen parameter - between (0 and 1]
double flea_param (double mx, double mn, double value);

