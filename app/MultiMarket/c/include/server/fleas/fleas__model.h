// Copyright 21-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from fleas->model

#ifndef SERVER_FLEAS_FLEAS__MODEL_H
  #define SERVER_FLEAS_FLEAS__MODEL_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *fleas__model_process(AsyncActor *ac, Map *mrq);

#endif
