// Copyright 06-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from sys->nicks

#ifndef SERVER_SYS_SYS__NICKS_H
  #define SERVER_SYS_SYS__NICKS_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *sys__nicks_process(AsyncActor *ac, Map *mrq);

#endif
