// Copyright 25-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[DocEntry*].

#ifndef DATA_DOCENTRY_ODOCENTRY_H
  #define DATA_DOCENTRY_ODOCENTRY_H

#include "data/DocEntry.h"

/// Opt[DocEntry*].
typedef struct oDocEntry_ODocEntry ODocEntry;

/// Returns a none option.
ODocEntry *oDocEntry_mk_none();

/// Returns an option with a value.
ODocEntry *oDocEntry_mk_some(DocEntry *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oDocEntry_none(ODocEntry *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
DocEntry *oDocEntry_some(ODocEntry *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
DocEntry *oDocEntry_esome (ODocEntry *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
DocEntry *oDocEntry_osome (ODocEntry *opt, DocEntry *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
DocEntry *oDocEntry_nsome (ODocEntry *opt);

/// Returns this JSONized.
///   this: Container.
char *oDocEntry_to_js (ODocEntry *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
ODocEntry *oDocEntry_from_js (char *js);


//--// Not remove

#endif