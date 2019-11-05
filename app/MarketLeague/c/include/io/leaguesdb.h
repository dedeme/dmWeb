// Copyright 27-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Leagues data base.

#ifndef IO_LEAGUESDB_H
  #define IO_LEAGUESDB_H

#include "dmc/std.h"

/// Arr<char> Returns sessions list.
Arr *leaguesdb_sessions (void);

/// Arr<char> Returns rounds list (NN, ..., 02, 01)
Arr *leaguesdb_rounds (char *session);

#endif
