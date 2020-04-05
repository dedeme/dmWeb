// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Main page of Settings.

#ifndef SERVER_SETTINGS_MAIN_H
  #define SERVER_SETTINGS_MAIN_H

#include "dmc/async.h"

/// Entry point to process requests.
char *mainSettings_process (AsyncActor *ac, Map *mrq);

#endif
