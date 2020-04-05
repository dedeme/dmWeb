// Copyright 19-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Management of fleas.

#ifndef SCHEDULER_FLEAS_H
  #define SCHEDULER_FLEAS_H

#include "dmc/async.h"
#include "data/flea/Fmodel.h"

/*--*/

/// Flea evaluation data.
///   Arguments:
///     flea: Flea
///     buys: int
///     sells: int
///     assets: double
///     profits: double
///     eval: double
typedef struct fleas_FleasEval FleasEval;

/// Evaluated flea.
Flea *fleasEval_flea (FleasEval *this);

/// Number of buys.
int fleasEval_buys (FleasEval *this);

/// Number of sells.
int fleasEval_sells (FleasEval *this);

/// Assets in simulation (money).
double fleasEval_assets (FleasEval *this);

/// Profits average of every company (ratios --can be < 0).
double fleasEval_profits (FleasEval *this);

/// Evalutarion result. Its value is between -1 and 1.
double fleasEval_eval (FleasEval *this);

///
Js *fleasEval_to_js (FleasEval *this);

///
FleasEval *fleasEval_from_js (Js *js);

/*--*/

/// Update fleas data.
void fleas_update (
  AsyncActor *ac,
  Qtable *opens,
  Qtable *closes,
  Fmodel *model,
  Opt *log_id // Opt[char]
);

/// Update fleas data in thread apart.
void fleas_run(AsyncActor *ac);

/// Returns Arr[FleasEval] with 'fleas' evaluated.
/// If 'filtered' != 0, fleas with sells out of bounds are removed.
Arr *fleas_evaluate (
  Qtable *opens,
  Qtable *closes,
  Fmodel *model,
  Arr *efleas, // Arr[FleasEval]
  int filtered
);

#endif
