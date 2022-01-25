// Copyright 24-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Results tables.

#ifndef DB_RESULTS_H
  #define DB_RESULTS_H

#include "data/Result/AAOResult.h"

///
void results_init (char *parent);

///
AAOResult *results_read (char *year);

///
void results_write (AAOResult *results);

#endif
