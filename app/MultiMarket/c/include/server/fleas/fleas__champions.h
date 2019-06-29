// Copyright 21-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from fleas->champions

#ifndef SERVER_FLEAS_FLEAS__CHAMPIONS_H
  #define SERVER_FLEAS_FLEAS__CHAMPIONS_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *fleas__champions_process(AsyncActor *ac, Map *mrq);

#endif
