// Copyright 05-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from sys->home

#ifndef SERVER_SYS_SYS__HOME_H
  #define SERVER_SYS_SYS__HOME_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *sys__home_process(AsyncActor *ac, Map *mrq);

#endif
