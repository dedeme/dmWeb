// Copyright 15-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Main entry point of www server.

#ifndef SERVER_HUB_H
  #define SERVER_HUB_H

#include "dmc/async.h"

/// Entry point to process requests.
char *hub_rp (AsyncActor *ac, char *rq);

#endif
