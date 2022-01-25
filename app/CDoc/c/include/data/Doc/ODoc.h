// Copyright 25-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[Doc*].

#ifndef DATA_DOC_ODOC_H
  #define DATA_DOC_ODOC_H

#include "data/Doc.h"

/// Opt[Doc*].
typedef struct oDoc_ODoc ODoc;

/// Returns a none option.
ODoc *oDoc_mk_none();

/// Returns an option with a value.
ODoc *oDoc_mk_some(Doc *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oDoc_none(ODoc *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
Doc *oDoc_some(ODoc *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
Doc *oDoc_esome (ODoc *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
Doc *oDoc_osome (ODoc *opt, Doc *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
Doc *oDoc_nsome (ODoc *opt);

/// Returns this JSONized.
///   this: Container.
char *oDoc_to_js (ODoc *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
ODoc *oDoc_from_js (char *js);


//--// Not remove

#endif