// Copyright 20-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/IndexTree.h"

/* .
# Dpath index tree.
IndexTree: to
  id: char *
  is_path: bool
  # If is_path is 0, 'doc' is an empty string.
  doc: char *
  # Opt[IndexTree] If is_path is 'true', 'tree' is 'null'.
  trees: Arr - IndexTree
*/

/*--*/

struct IndexTree_IndexTree {
  char *id;
  int is_path;
  char *doc;
  Arr *trees;
};

IndexTree *indexTree_new (
  char *id,
  int is_path,
  char *doc,
  Arr *trees
) {
  IndexTree *this = MALLOC(IndexTree);
  this->id = id;
  this->is_path = is_path;
  this->doc = doc;
  this->trees = trees;
  return this;
}

char *indexTree_id (IndexTree *this) {
  return this->id;
}

int indexTree_is_path (IndexTree *this) {
  return this->is_path;
}

char *indexTree_doc (IndexTree *this) {
  return this->doc;
}

Arr *indexTree_trees (IndexTree *this) {
  return this->trees;
}

Js *indexTree_to_js (IndexTree *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(this->id));
  arr_push(js, js_wb(this->is_path));
  arr_push(js, js_ws(this->doc));
  arr_push(js, arr_to_js(this->trees, (FTO)indexTree_to_js));
  return js_wa(js);
}

/*--*/

IndexTree *indexTree_empty (void) {
  return indexTree_new("", 1, "", arr_new());
}
