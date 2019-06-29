// Copyright 23-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Entry point of acc pages requests.

#ifndef SERVER_ACC_H
  #define SERVER_ACC_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *acc_process(AsyncActor *ac, Map *mrq);

#endif
