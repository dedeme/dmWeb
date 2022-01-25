// Copyright 08-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/IndexTree.h"
#include "dmc/DEFS.h"
#include "dmc/js.h"
#include "dmc/err.h"
#include "dmc/char/Achar.h"
#include "data/IndexTree/AIndexTree.h"

IndexTree *indexTree_new (char *id, Ochar *doc, AIndexTree *trees) {
  IndexTree *this = MALLOC(IndexTree);
  this->id = id;
  this->doc = doc;
  this->trees = trees;
  return this;
}

char *indexTree_to_js (IndexTree *this) {
  return js_wa(achar_new_from(
    js_ws(this->id),
    ochar_to_js(this->doc),
    aIndexTree_to_js(this->trees),
    NULL
  ));
}

IndexTree *indexTree_from_js (char *js) {
  return FAIL("Unimplemented function");
}
