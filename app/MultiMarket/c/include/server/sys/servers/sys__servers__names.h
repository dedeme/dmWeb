// Copyright 20-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from sys->servers->names

#ifndef SERVER_SYS_SERVERS_SYS__SERVERS__NAMES_H
  #define SERVER_SYS_SERVERS_SYS__SERVERS__NAMES_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *sys__servers__names_process(AsyncActor *ac, Map *mrq);

#endif
