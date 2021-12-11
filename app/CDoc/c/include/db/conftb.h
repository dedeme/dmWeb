// Copyright 08-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Configuration table.

#ifndef DB_CONFTB_H
  #define DB_CONFTB_H

#include "data/Conf.h"

/// Initializes table.
void conftb_init (char *parent);

/// Read configuration in JSON fomat.
char *conftb_read_js (void);

/// Read configuration.
Conf *conftb_read (void);

/// Write configuration
void conftb_write (Conf *cf);

#endif
