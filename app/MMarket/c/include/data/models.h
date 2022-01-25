// Copyright 16-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Models register.

#ifndef DATA_MODELS_H
  #define DATA_MODELS_H

#include "data/Model/AModel.h"

/// Returns Models list.
AModel *models_list (void);

/// Returns Model ids list.
Achar *models_ids_list (void);

/// Returns a model from its 'id'
Model *models_get(char *id);

#endif
