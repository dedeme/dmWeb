// Copyright 13-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[LogEntry*].

#ifndef DATA_LOGENTRY_OLOGENTRY_H
  #define DATA_LOGENTRY_OLOGENTRY_H

#include "data/LogEntry.h"

/// Opt[LogEntry*].
typedef struct oLogEntry_OLogEntry OLogEntry;

/// Returns a none option.
OLogEntry *oLogEntry_mk_none();

/// Returns an option with a value.
OLogEntry *oLogEntry_mk_some(LogEntry *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oLogEntry_none(OLogEntry *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
LogEntry *oLogEntry_some(OLogEntry *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
LogEntry *oLogEntry_esome (OLogEntry *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
LogEntry *oLogEntry_osome (OLogEntry *opt, LogEntry *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
LogEntry *oLogEntry_nsome (OLogEntry *opt);

/// Returns this JSONized.
///   this: Container.
///   to  : Converter of container element to JSON.
char *oLogEntry_to_js (OLogEntry *this, char *(*to)(LogEntry *e));

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
///   from: Converter from JSON to container element.
OLogEntry *oLogEntry_from_js (char *js, LogEntry *(*from)(char *ejs));


//--// Not remove

#endif