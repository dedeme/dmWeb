// Copyright 13-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Log entry data.

#ifndef DATA_LOGENTRY_H
  #define DATA_LOGENTRY_H

/// Log entry data.
struct logEntry_LogEntry {
  char *type;
  char *time;
  char *msg;
};

/// Log entry data.
typedef struct logEntry_LogEntry LogEntry;

/// Constructor.
/// Message of type "F" only must be constructed by function 'Kron_err'.
/// To construct Information and message entries is better to use
/// 'log_info' and 'log_msg', if it is possible.
///   type: F(ail), I(nformation), M(essage).
///   msg : Message to show.
LogEntry *logEntry_new (char *type, char *msg);

///
char *logEntry_to_js (LogEntry *e);

///
LogEntry *logEntry_from_js (char *js);

#endif
