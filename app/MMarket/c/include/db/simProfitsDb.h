// Copyright 28-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Simulations profits database.

#include "data/simulation/SimProfitsData.h"

#ifndef DB_SIMPROFITSDB_H
  #define DB_SIMPROFITSDB_H

/// Initializes data base.
void simProfitsDb_init (char *parent);

/// Reads Simulations profits database of a model.
SimProfitsData *simProfitsDb_read(char *model_id);

/// Writes Simulations profits database of a model.
void simProfitsDb_write(char *model_id, SimProfitsData *data);

#endif
