// Copyright 08-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Doc.h"
#include "dmc/DEFS.h"
#include "dmc/js.h"
#include "dmc/err.h"
#include "dmc/char/Achar.h"

Doc *doc_new (char *doc) {
  Doc *this = MALLOC(Doc);
  this->doc = doc;
  this->defines = aDocEntry_new();
  this->enums = aDocEntry_new();
  this->structs = aDocEntry_new();
  this->typedefs = aDocEntry_new();
  this->unions = aDocEntry_new();
  this->functions = aDocEntry_new();
  this->vars = aDocEntry_new();
  return this;
}

char *doc_to_js (Doc *this) {
  return js_wa(achar_new_from(
    js_ws(this->doc),
    js_wa((Achar *)aDocEntry_map(
      this->defines, (void *(*)(DocEntry *))docEntry_to_js
    )),
    js_wa((Achar *)aDocEntry_map(
      this->enums, (void *(*)(DocEntry *))docEntry_to_js
    )),
    js_wa((Achar *)aDocEntry_map(
      this->structs, (void *(*)(DocEntry *))docEntry_to_js
    )),
    js_wa((Achar *)aDocEntry_map(
      this->typedefs, (void *(*)(DocEntry *))docEntry_to_js
    )),
    js_wa((Achar *)aDocEntry_map(
      this->unions, (void *(*)(DocEntry *))docEntry_to_js
    )),
    js_wa((Achar *)aDocEntry_map(
      this->functions, (void *(*)(DocEntry *))docEntry_to_js
    )),
    js_wa((Achar *)aDocEntry_map(
      this->vars, (void *(*)(DocEntry *))docEntry_to_js
    )),
    NULL
  ));
}

Doc *doc_from_js(char *js) {
  return FAIL("Unimplemented function");
}

