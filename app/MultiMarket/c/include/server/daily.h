// Copyright 28-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Entry point of daily requests.

#ifndef SERVER_DAILY_H
  #define SERVER_DAILY_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *daily_process(AsyncActor *ac, Map *mrq);

#endif
