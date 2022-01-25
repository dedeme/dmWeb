// Copyright 24-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Points tables.

#ifndef DB_POINTS_H
  #define DB_POINTS_H

#include "data/Result/AAOResult.h"

///
void points_init (char *parent);

///
AAOResult *points_read (char *year);

///
void points_write (AAOResult *points);

#endif
