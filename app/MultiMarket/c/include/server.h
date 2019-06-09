// Copyright 04-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Server main.

#ifndef SERVER_H
  #define SERVER_H

#include "dmc/async.h"
#include "dmc/Iserver.h"

/// 'server_actor' is Tp[AsyncActor, Iserver]
void server_run (Tp *actor_server);

#endif
