// Copyright 19-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Orders page of Fleas Ftests.

#ifndef SERVER_FLEAS_FTESTS_ORDERS_H
  #define SERVER_FLEAS_FTESTS_ORDERS_H

#include "dmc/async.h"

/// Entry point to process requests.
char *orders_process (AsyncActor *ac, Map *mrq);

#endif
