// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Settings page of Settings

#ifndef SERVER_SETTINGS_SETTINGS_H
  #define SERVER_SETTINGS_SETTINGS_H

#include "dmc/async.h"

/// Entry point to process requests.
char *settings_process (AsyncActor *ac, Map *mrq);

#endif
