// Copyright 08-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Paths table.

#ifndef DB_PATHS_H
  #define DB_PATHS_H

#include "data/Dpath/ADpath.h"

/// Initializes table.
void dpaths_init (char *parent);

/// Read configuration and verifies path existence.
ADpath *dpaths_read (void);

/// Write configuration
void dpaths_write (ADpath *paths);

#endif
