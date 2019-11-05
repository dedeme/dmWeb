// Copyright 26-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Backups management.

#ifndef IO_BACKUPSDB_H
  #define IO_BACKUPSDB_H

#include "dmc/std.h"

/// Arr<char> Returns the list of all the backups.
Arr *backupsdb_backups (void);

/// Arr<char> Returns the list of all the elements in trash.
Arr *backupsdb_trash (void);

/// Returns backup name (placed in 'tmp' directory)
char *backupsdb_mkbackup (void);

/// Starts restore.
void backupsdb_restore_start();

/// Aborts restore.
void backupsdb_restore_abort();

/// Adds data to copy. 'data' is a B64 string.
void backupsdb_restore_append(char *data);

/// Finishes restore.
///   return: "wrong" -> File 'version.txt' is not the expected one.
///           "missing" -> File 'version.txt' is missing.
///           "" -> Result is ok.
char *backupsdb_restore_end (void);

/// Clear trash.
void backupsdb_clear_trash (void);

/// Restores a backup from trash.
///   file: The name of file including extension .zip.
void backupsdb_restore_trash (char *file);

/// Automatic backup.
void backupsdb_mkautobackup (void);

/// Automatic restore.
void backupsdb_autorestore (char *file);

#endif
