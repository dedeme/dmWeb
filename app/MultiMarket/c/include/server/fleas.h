// Copyright 19-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Entry point of fleas pages requests.

#ifndef SERVER_FLEAS_H
  #define SERVER_FLEAS_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *fleas_process(AsyncActor *ac, Map *mrq);

#endif
