// Copyright 17-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Model evaluation data.

#ifndef DATA_EVAL_MODELEVAL_H
  #define DATA_EVAL_MODELEVAL_H

#include "dmc/ADouble.h"

/// Model evaluation data.
struct modelEval_ModelEval {
  /// Params evaluated.
  ADouble *params;
  /// Weeks weight to calculate historic values.
  int weeks;
  /// Historic value.
  double hvalue;
  /// Historic sales.
  double hsales;
  /// Last value.
  double value;
  /// Last sales.
  double sales;
};

/// Model evaluation data.
typedef struct modelEval_ModelEval ModelEval;

/// Constructor
ModelEval *modelEval_new (
  ADouble *params, int weeks,
  double hvalue, double hsales, double value, double sales
);

///
char *modelEval_to_js(ModelEval *this);

///
ModelEval *modelEval_from_js(char *js);

#endif
