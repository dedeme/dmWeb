// Copyright 05-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef CORE_BACKUPS_H
  #define CORE_BACKUPS_H

#include "dmc/std.h"

///
void backups_process(
  const char *app_name,
  const char *data_version,
  // Map[Js]
  Map *rq
);

#endif
