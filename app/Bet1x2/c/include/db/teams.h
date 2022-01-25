// Copyright 24-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Teams tables.

#ifndef DB_TEAMS_H
  #define DB_TEAMS_H

#include "dmc/char/AKchar.h"
#include "dmc/char/Achar.h"

///
void teams_init (char *parent);

///
AKchar *teams_read (char *year);

/// Retuns years with data.
Achar *teams_years (void);

#endif
