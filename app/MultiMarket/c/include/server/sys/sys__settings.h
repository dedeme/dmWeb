// Copyright 05-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from sys->settings

#ifndef SERVER_SYS_SYS__SETTINGS_H
  #define SERVER_SYS_SYS__SETTINGS_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *sys__settings_process(AsyncActor *ac, Map *mrq);

#endif
