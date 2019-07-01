// Copyright 01-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from sys->annotations

#ifndef SERVER_SYS_SYS__ANNOTATIONS_H
  #define SERVER_SYS_SYS__ANNOTATIONS_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *sys__annotations_process(AsyncActor *ac, Map *mrq);


#endif
