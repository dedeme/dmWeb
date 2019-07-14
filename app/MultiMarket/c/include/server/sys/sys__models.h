// Copyright 12-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from sys->models

#ifndef SERVER_SYS_SYS__MODELS_H
  #define SERVER_SYS_SYS__MODELS_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *sys__models_process(AsyncActor *ac, Map *mrq);

#endif
