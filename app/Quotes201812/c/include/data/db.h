// Copyright 10-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_DB_H
  #define DATA_DB_H

#include "dmc/std.h"

/// 'path' is the path of the directory 'data'. Only runs the first time that
/// the application is launched.
void db_create(char *path);

/// 'path' is the path of the directory 'data'. Runs every time thah the
/// application is launched.
void db_init(char *path);

/// Return the directory 'data'
char *db_dir(void);

#endif
