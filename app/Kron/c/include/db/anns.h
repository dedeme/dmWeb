// Copyright 14-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Annotations table.

#ifndef DB_ANNS_H
  #define DB_ANNS_H

#include "data/Ann/AAnn.h"

/// Initializes table.
void anns_init (char *parent);

/// Read annotations.
AAnn *anns_read (void);

/// Write annotations.
void anns_write (AAnn *anns);

#endif
