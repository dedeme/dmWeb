// Copyright 15-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Server main.

#ifndef SERVER_H
  #define SERVER_H

#include "dmc/async.h"
#include "dmc/Iserver.h"

///
void server_run (AsyncActor *ac, Iserver *server);

#endif
