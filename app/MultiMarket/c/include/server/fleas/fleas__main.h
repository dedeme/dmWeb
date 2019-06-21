// Copyright 19-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from fleas->main

#ifndef SERVER_FLEAS_FLEAS__MAIN_H
  #define SERVER_FLEAS_FLEAS__MAIN_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *fleas__main_process(AsyncActor *ac, Map *mrq);

#endif
