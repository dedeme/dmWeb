// Copyright 22-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Annotations table.

#ifndef DB_EXES_H
  #define DB_EXES_H

#include "data/Exe/AExe.h"

/// Initializes table.
void exes_init (char *parent);

/// Read execution annotations data.
AExe *exes_read (void);

/// Write execution annotations data.
void exes_write (AExe *exes);

#endif
