// Copyright 05-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Trash management.

#ifndef IO_TRASH_H
  #define IO_TRASH_H

#include "dmc/async.h"

///
void trash_init (void);

/// Arr[char]
Arr *trash_list (void);

/// Zips directory data in trash directory.
void trash_zip_data (void);

/// Clears trash directory
void trash_clear (void);

/// Restores a file in trash directory.
void trash_restore (char *file);

#endif
