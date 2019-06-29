// Copyright 24-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from acc->annotations

#ifndef SERVER_ACC_ACC__ANNOTATIONS_H
  #define SERVER_ACC_ACC__ANNOTATIONS_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *acc__annotations_process(AsyncActor *ac, Map *mrq);

#endif
