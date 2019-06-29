// Copyright 27-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from acc->trading

#ifndef SERVER_ACC_ACC__TRADING_H
  #define SERVER_ACC_ACC__TRADING_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *acc__trading_process(AsyncActor *ac, Map *mrq);

#endif
