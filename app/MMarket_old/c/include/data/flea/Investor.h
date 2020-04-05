// Copyright 22-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Record of Model and Flea.

#ifndef DATA_FLEA_INVESTOR_H
  #define DATA_FLEA_INVESTOR_H

#include "dmc/async.h"

#include "data/flea/Flea.h"
#include "data/flea/Fmodel.h"
#include "scheduler/fleas.h"

/*--*/

/// Record of Model and FleasEval.
///   Arguments:
///     model: Fmodel
///     eflea: FleasEval
typedef struct Investor_Investor Investor;

///
Investor *investor_new (Fmodel *model, FleasEval *eflea);

///
Fmodel *investor_model (Investor *this);

///
FleasEval *investor_eflea (Investor *this);

/*--*/

///
Js *investor_to_js (Investor *this);

/// Returns Opt[Investor]
Opt *investor_from_js_opt (Js *js);

#endif
