// Copyright 17-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Model range evaluation data.

#ifndef DATA_EVAL_MODELEVALS_H
  #define DATA_EVAL_MODELEVALS_H

#include "data/eval/ModelEval/AModelEval.h"

/// Model range evaluation data.
struct modelEvals_ModelEvals {
  // Date of last evaluation.
  char *date;
  // Evaluations
  AModelEval *evals;
};

/// Model range evaluation data.
typedef struct modelEvals_ModelEvals ModelEvals;

/// Constructor
ModelEvals *modelEvals_new (char *date, AModelEval *evals);

///
char *modelEvals_to_js(ModelEvals *this);

///
ModelEvals *modelEvals_from_js(char *js);

#endif
