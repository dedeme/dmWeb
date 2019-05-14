// Copyright 03-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef IO_LOG_H
  #define IO_LOG_H

#include "dmc/async.h"

/// Intializes 'Log'
void log_init (void);

/// Writes an error
void log_error (char *msg);

/// Writes a warning
void log_warning (char *msg);

/// Writes a exception.
///   stack is Arr[char]
void log_exception (char *msg, Arr *stack);

/// Returns an array of messages 'JSONized'
Js *log_to_js (void);

#endif
