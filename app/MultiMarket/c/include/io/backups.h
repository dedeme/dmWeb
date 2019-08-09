// Copyright 05-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Backups management.

#ifndef IO_BACKUPS_H
  #define IO_BACKUPS_H

#include "dmc/async.h"

///
void backups_init (void);

/// Arr[char]
Arr *backups_list (void);

/// Makes a backup in a zip file and returns its name (including extension
/// .zip)
char *backups_make (void);

/// Makes an automatic backup
void backups_make_automatic (void);

/// Starts restore process, creating an empty "back.zip".
void backups_restore_start (void);

/// Appends binary data in B64 format to file "back.zip" created with
/// 'backups_restore_start'
void backups_restore_append (char *data);

/// Aborts a restore process
void backups_restore_abort (void);

/// Finalizes restore process an returning: "" if there is no error,
/// "missing" if there is not 'version.txt' in the backup file or "wrong"
/// the content of 'version.txt' is not valid.
char *backups_restore_end (void);

/// Restores an automatic backup
void backups_autorestore (char *file);

#endif
