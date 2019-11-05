// Copyright 17-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Program definitions

#ifndef DEFS_H
  #define DEFS_H

#include "dmc/std.h"

///
#define CONF_FILE "data/conf.db"

///
#define NICKS_FILE "/home/deme/.dmCApp/MultiMarket/data/nicks.db"

///
#define DAILY_QUOTES_DIR "/home/deme/.dmCApp/MultiMarket/data/daily"

///
#define HISTORIC_QUOTES_DIR "/home/deme/.dmCApp/MultiMarket/data/quotes"

///
#define SHORT_NQUOTES 50

///
#define MEDIUM_NQUOTES 100

///
#define LONG_NQUOTES 200

/// Groups
enum LeagueGroup {DAILY_G, SHORT_G, MEDIUM_G, LONG_G};

#endif
