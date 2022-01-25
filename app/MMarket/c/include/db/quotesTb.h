// Copyright 15-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Quotes table.

#ifndef DB_QUOTESTB_H
  #define DB_QUOTESTB_H

#include "data/Quotes.h"

/// Initializes data base.
void quotesTb_init (char *parent);

/// Read quotes.
Quotes *quotesTb_read(void);

/// Write quotes.
void quotesTb_write(Quotes *qs);

#endif
