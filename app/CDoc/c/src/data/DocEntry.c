// Copyright 08-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/DocEntry.h"
#include "dmc/DEFS.h"
#include "dmc/js.h"
#include "dmc/err.h"
#include "dmc/char/Achar.h"

DocEntry *docEntry_new (
  char *name,
  char *doc,
  char *code,
  char *link
) {
  DocEntry *this = MALLOC(DocEntry);
  this->name = name;
  this->doc = doc;
  this->code = code;
  this->link = link;
  return this;
}

char *docEntry_to_js (DocEntry *this) {
  return js_wa(achar_new_from(
    js_ws(this->name),
    js_ws(this->doc),
    js_ws(this->code),
    js_ws(this->link),
    NULL
  ));
}

DocEntry *docEntry_from_js (char *js) {
  return FAIL("Unimplemented function");
}
