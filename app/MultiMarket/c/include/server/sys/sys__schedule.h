// Copyright 08-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef SERVER_SYS_SYS__SCHEDULE_H
  #define SERVER_SYS_SYS__SCHEDULE_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *sys__schedule_process(AsyncActor *ac, Map *mrq);

#endif
