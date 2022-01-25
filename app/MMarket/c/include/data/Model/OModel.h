// Copyright 16-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[Model*].

#ifndef DATA_MODEL_OMODEL_H
  #define DATA_MODEL_OMODEL_H

#include "data/Model.h"

/// Opt[Model*].
typedef struct oModel_OModel OModel;

/// Returns a none option.
OModel *oModel_mk_none();

/// Returns an option with a value.
OModel *oModel_mk_some(Model *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oModel_none(OModel *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
Model *oModel_some(OModel *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
Model *oModel_esome (OModel *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
Model *oModel_osome (OModel *opt, Model *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
Model *oModel_nsome (OModel *opt);

/// Returns this JSONized.
///   this: Container.
char *oModel_to_js (OModel *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OModel *oModel_from_js (char *js);


//--// Not remove

#endif