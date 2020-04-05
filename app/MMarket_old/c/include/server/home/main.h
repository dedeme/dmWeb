// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Main page of Home.

#ifndef SERVER_HOME_MAIN_H
  #define SERVER_HOME_MAIN_H

#include "dmc/async.h"

/// Entry point to process requests.
char *mainHome_process (AsyncActor *ac, Map *mrq);

#endif
