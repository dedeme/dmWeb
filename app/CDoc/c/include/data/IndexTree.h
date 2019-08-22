// Copyright 20-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Documentation path index tree.

#ifndef DATA_INDEXTREE_H
  #define DATA_INDEXTREE_H

#include "dmc/std.h"

/*--*/

/// Dpath index tree.
///   Arguments:
///     id: char*
///     is_path: bool
///     doc: char*
///     trees: Arr-IndexTree
typedef struct IndexTree_IndexTree IndexTree;

///
IndexTree *indexTree_new (
  char *id,
  int is_path,
  char *doc,
  Arr *trees
);

///
char *indexTree_id (IndexTree *this);

///
int indexTree_is_path (IndexTree *this);

/// If is_path is 0, 'doc' is an empty string.
char *indexTree_doc (IndexTree *this);

/// Opt[IndexTree] If is_path is 'true', 'tree' is 'null'.
Arr *indexTree_trees (IndexTree *this);

///
Js *indexTree_to_js (IndexTree *this);

/*--*/

IndexTree *indexTree_empty (void);

#endif
