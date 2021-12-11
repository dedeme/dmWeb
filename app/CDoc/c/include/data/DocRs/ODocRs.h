// Copyright 11-Dec-2021 ºDeme
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
///   to  : Converter of container element to JSON.
char *oDocRs_to_js (ODocRs *this, char *(*to)(DocRs *e));

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
///   from: Converter from JSON to container element.
ODocRs *oDocRs_from_js (char *js, DocRs *(*from)(char *ejs));


//--// Not remove

#endif