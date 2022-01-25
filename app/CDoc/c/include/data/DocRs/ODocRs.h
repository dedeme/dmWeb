// Copyright 25-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[DocRs*].

#ifndef DATA_DOCRS_ODOCRS_H
  #define DATA_DOCRS_ODOCRS_H

#include "data/DocRs.h"

/// Opt[DocRs*].
typedef struct oDocRs_ODocRs ODocRs;

/// Returns a none option.
ODocRs *oDocRs_mk_none();

/// Returns an option with a value.
ODocRs *oDocRs_mk_some(DocRs *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oDocRs_none(ODocRs *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
DocRs *oDocRs_some(ODocRs *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
DocRs *oDocRs_esome (ODocRs *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
DocRs *oDocRs_osome (ODocRs *opt, DocRs *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
DocRs *oDocRs_nsome (ODocRs *opt);

/// Returns this JSONized.
///   this: Container.
char *oDocRs_to_js (ODocRs *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
ODocRs *oDocRs_from_js (char *js);


//--// Not remove

#endif