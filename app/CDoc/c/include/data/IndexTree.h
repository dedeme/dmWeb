// Copyright 08-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Entry of index.
/// It can be a file path or a directory path.

#ifndef DATA_INDEXTREE_H
  #define DATA_INDEXTREE_H

#include "dmc/char/Ochar.h"

/// Entry of index.
typedef struct IndexTree_IndexTree IndexTree;

/// Arr[IndexTree *].
typedef struct aIndexTree_AIndexTree AIndexTree;

/// Entry of index.
/// It can be a file path or a directory path.
struct IndexTree_IndexTree {
  char *id; // Index entry identifier.
  Ochar *doc; // Documentation, if the entry is a file path.
              // Otherwise its value is none.
  AIndexTree *trees;
};

///
IndexTree *indexTree_new (char *id, Ochar *doc, AIndexTree *trees);

///
char *indexTree_to_js (IndexTree *this);

#endif
