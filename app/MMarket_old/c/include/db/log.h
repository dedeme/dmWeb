// Copyright 15-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// General log.

#ifndef DB_LOG_H
  #define DB_LOG_H

#include "dmc/async.h"

/// Initializes global log.
void log_init (void);

/// Resets log (Delete its contents)
void log_reset (void);

/// Adds an error message.
void log_error (char *msg);

/// Adds a normal message.
void log_info (char *msg);

/// Adds an error message from a exception.
void log_exception (Exc *ex);

/// Reads Log and returns an js-array of'JSONized' LogEntry's
Js *log_read (void);

#endif
