// Copyright 19-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Standar parameters for invesions.

#ifndef DATA_FLEA_FLEA_H
  #define DATA_FLEA_FLEA_H

#include "dmc/async.h"
#include "Gen.h"

/*--*/

/// Standar parameters for investments.
///   Arguments:
///     date: char*
///     cycle: int
///     id: int
///     gen: Gen
typedef struct Flea_Flea Flea;

///
Flea *flea_new (
  char *date,
  int cycle,
  int id,
  Gen *gen
);

/// Date of creation.
char *flea_date (Flea *this);

/// Cycle of creation
int flea_cycle (Flea *this);

/// Identifier number in cycle.
int flea_id (Flea *this);

/// Parameters of investment.
Gen *flea_gen (Flea *this);

///
Js *flea_to_js (Flea *this);

///
Flea *flea_from_js (Js *js);

/*--*/

/// Returns date-cycle-id
char *flea_name (Flea *this);

/// Calculates value of a gen parameter.
///   mx   : Maximun value of parameter
///   mn   : Minimum value of parameter
///   value: Gen parameter - between (0 and 1]
///   - If mx <= mn returns 0.
double flea_param (double mx, double mn, double value);

/// Two fleas are equals if they have equals gen.
int flea_eq (Flea *this, Flea *other);

/// Evaluate a flea.
///   this   : The flea.
///   today  : Current date (from 'date_now').
///   assets : Ratio of assets
///   profits: Ratio of profits.
///   return : A ponderation of 'assets', 'profits' and age.
double flea_evaluate (Flea *this, time_t today, double assets, double profits);

#endif
