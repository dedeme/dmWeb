// Copyright 08-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[IndexTree*].

#ifndef DATA_INDEXTREE_OINDEXTREE_H
  #define DATA_INDEXTREE_OINDEXTREE_H

#include "data/IndexTree.h"

/// Opt[IndexTree*].
typedef struct oIndexTree_OIndexTree OIndexTree;

/// Returns a none option.
OIndexTree *oIndexTree_mk_none();

/// Returns an option with a value.
OIndexTree *oIndexTree_mk_some(IndexTree *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oIndexTree_none(OIndexTree *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
IndexTree *oIndexTree_some(OIndexTree *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
IndexTree *oIndexTree_esome (OIndexTree *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
IndexTree *oIndexTree_osome (OIndexTree *opt, IndexTree *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
IndexTree *oIndexTree_nsome (OIndexTree *opt);

/// Returns this JSONized.
///   this: Container.
///   to  : Converter of container element to JSON.
char *oIndexTree_to_js (OIndexTree *this, char *(*to)(IndexTree *e));

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
///   from: Converter from JSON to container element.
OIndexTree *oIndexTree_from_js (char *js, IndexTree *(*from)(char *ejs));


//--// Not remove

#endif