// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Calendar page of Settings.

#ifndef SERVER_SETTINGS_CALENDAR_H
  #define SERVER_SETTINGS_CALENDAR_H

#include "dmc/async.h"

/// Entry point to process requests.
char *calendar_process (AsyncActor *ac, Map *mrq);

#endif
