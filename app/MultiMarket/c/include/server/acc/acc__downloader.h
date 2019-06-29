// Copyright 25-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from widget Downloader of acc.

#ifndef SERVER_ACC_ACC__DOWNLOADER_H
  #define SERVER_ACC_ACC__DOWNLOADER_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *acc__downloader_process(AsyncActor *ac, Map *mrq);

#endif
