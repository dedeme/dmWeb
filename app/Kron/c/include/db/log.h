// Copyright 13-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Configuration table.

#ifndef DB_LOG_H
  #define DB_LOG_H

#include "data/LogEntry/ALogEntry.h"

/// Initializes table.
void log_init (char *parent);

/// Read log in JSON fomat.
char *log_read_js (void);

/// Read log.
ALogEntry *log_read (void);

/// Write log
void log_write (ALogEntry *log);

/// Add an info entry.
void log_info (char *msg);

/// Add an message entry.
void log_msg (char *msg);

#endif
