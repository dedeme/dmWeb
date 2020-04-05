// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Fleas log.

#ifndef DB_FLEAS_FLOG_H
  #define DB_FLEAS_FLOG_H

#include "dmc/async.h"

/// Initializes fleas log.
void flog_init (void);

/// Resets flog (Delete its contents) and generates a new id. Such id is
/// returned.
char *flog_new (void);

/// Stops flog.
/// If identifier is out of date, this function do nothing.
///   id : Identifier returned by flog_new. If its value not mach, this
///        function do nothing.
void flog_stop (char *id);

/// Adds a normal message.
/// If identifier is out of date, this function do nothing.
///   id : Identifier returned by flog_new. If its value not mach, no
///        annotations will be done.
///   msg: Messages to show.
void flog_info (char *id, char *msg);

/// Reads Log and returns an 'JSONized' Opt[Arr[LogEntry's]]
/// If identifier is out of date, this function do nothing.
///   id: Identifier returned by flog_new. If its value not mach, the 'JSONized'
///       Opt[Arr[LogEntry's]] will be empty.
Js *flog_read (char *id);

#endif
