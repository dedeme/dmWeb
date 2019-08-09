// Copyright 07-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Entry point of ranking requests.

#ifndef SERVER_RANKING_H
  #define SERVER_RANKING_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *ranking_process(AsyncActor *ac, Map *mrq);

#endif
