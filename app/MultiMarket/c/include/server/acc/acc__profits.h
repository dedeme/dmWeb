// Copyright 27-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from acc->profits

#ifndef SERVER_ACC_ACC__PROFITS_H
  #define SERVER_ACC_ACC__PROFITS_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *acc__profits_process(AsyncActor *ac, Map *mrq);

#endif
