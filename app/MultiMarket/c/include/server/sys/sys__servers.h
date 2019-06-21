// Copyright 11-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from sys->servers

#ifndef SERVER_SYS_SYS__SERVERS_H
  #define SERVER_SYS_SYS__SERVERS_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *sys__servers_process(AsyncActor *ac, Map *mrq);

#endif
