// Copyright 20-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Results of model_assets

#ifndef DATA_FLEA_RESULTS_RS_H
  #define DATA_FLEA_RESULTS_RS_H

#include "dmc/async.h"

/*--*/

/// Results of model_assets
///   Arguments:
///     assets: double
///     buys: int
///     sells: int
typedef struct Rs_Rs Rs;

///
Rs *rs_new (double assets, int buys, int sells);

/// Amount
double rs_assets (Rs *this);

/// Buys number
int rs_buys (Rs *this);

/// Sells number
int rs_sells (Rs *this);

///
Js *rs_to_js (Rs *this);

/*--*/

#endif
