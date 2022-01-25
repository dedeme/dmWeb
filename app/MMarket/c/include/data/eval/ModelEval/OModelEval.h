// Copyright 17-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[ModelEval*].

#ifndef DATA_EVAL_MODELEVAL_OMODELEVAL_H
  #define DATA_EVAL_MODELEVAL_OMODELEVAL_H

#include "data/eval/ModelEval.h"

/// Opt[ModelEval*].
typedef struct oModelEval_OModelEval OModelEval;

/// Returns a none option.
OModelEval *oModelEval_mk_none();

/// Returns an option with a value.
OModelEval *oModelEval_mk_some(ModelEval *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oModelEval_none(OModelEval *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
ModelEval *oModelEval_some(OModelEval *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
ModelEval *oModelEval_esome (OModelEval *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
ModelEval *oModelEval_osome (OModelEval *opt, ModelEval *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
ModelEval *oModelEval_nsome (OModelEval *opt);

/// Returns this JSONized.
///   this: Container.
char *oModelEval_to_js (OModelEval *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OModelEval *oModelEval_from_js (char *js);


//--// Not remove

#endif