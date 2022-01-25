// Copyright 17-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Evaluations database.

#include "data/eval/ModelEvals.h"

#ifndef DB_EVALSDB_H
  #define DB_EVALSDB_H

/// Initializes data base.
void evalsDb_init (char *parent);

/// Reads evaluations data of a model.
ModelEvals *evalsDb_read(char *model_id);

/// Writes evalutations data of a model.
void evalsDb_write(char *model_id, ModelEvals *evals);

#endif
