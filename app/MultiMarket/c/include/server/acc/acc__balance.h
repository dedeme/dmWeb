// Copyright 26-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from acc->balance

#ifndef SERVER_ACC_ACC__BALANCE_H
  #define SERVER_ACC_ACC__BALANCE_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *acc__balance_process(AsyncActor *ac, Map *mrq);

#endif
