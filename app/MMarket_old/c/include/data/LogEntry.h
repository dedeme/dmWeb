// Copyright 15-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Entry of logs.

#ifndef DATA_LOGENTRY_H
  #define DATA_LOGENTRY_H

#include "dmc/async.h"

/*--*/

/// Logs entry
///   Arguments:
///     is_error: int
///     time: char*
///     msg: char*
typedef struct LogEntry_LogEntry LogEntry;

///
LogEntry *logEntry_new (int is_error, char *time, char *msg);

/// 1 if 'this' is a error entry.
int logEntry_is_error (LogEntry *this);

/// Time of message. Its format is 'dd/mm/yyyy(hh:mm:ss)'.
char *logEntry_time (LogEntry *this);

/// Log message.
char *logEntry_msg (LogEntry *this);

///
Js *logEntry_to_js (LogEntry *this);

///
LogEntry *logEntry_from_js (Js *js);

/*--*/

#endif
