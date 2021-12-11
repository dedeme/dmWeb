// Copyright 11-Dec-2021 ºDeme
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
///   to  : Converter of container element to JSON.
char *oDoc_to_js (ODoc *this, char *(*to)(Doc *e));

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
///   from: Converter from JSON to container element.
ODoc *oDoc_from_js (char *js, Doc *(*from)(char *ejs));


//--// Not remove

#endif