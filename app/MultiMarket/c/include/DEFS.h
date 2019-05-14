// Copyright 03-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DEFS_H
  #define DEFS_H

#include "dmc/async.h"

///
#define APP_NAME "MultiMarket"

/// Log maximum entries
#define LOG_MAX_ENTRIES 1000

/// Communications port
#define PORT 50286

/// Time to server sleep (milliseconds)
#define ACTOR_SLEEP 50

/// Time to server sleep (milliseconds)
#define SERVER_SLEEP 50

/// Time to scheduler sleep (milliseconds)
#define SCHEDULER_SLEEP 50

/// Time of connection expiration
#define EXPIRATION 900

/// Data version
#define DATA_VERSION "MultiMarket\nData version: 201905\n"

/// Number of quotes in historic
#define HISTORIC_QUOTES 750

#endif
