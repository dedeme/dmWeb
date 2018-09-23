// Copyright 07-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Mdoc.h"

/*.
struct: Mdoc
  doc: char *: _string
  functions: ApackDoc *: _array packDoc
  classes: Aclass *: _array class
*/
/*.-.*/
#include "dmc/ct/Ajson.h"

struct mdoc_Mdoc {
  char *doc;
  ApackDoc *functions;
  Aclass *classes;
};

Mdoc *mdoc_new(char *doc, ApackDoc *functions, Aclass *classes) {
  Mdoc *this = MALLOC(Mdoc);
  XNULL(doc)
  this->doc = doc;
  XNULL(functions)
  this->functions = functions;
  XNULL(classes)
  this->classes = classes;
  return this;
}

char *mdoc_doc(Mdoc *this) {
  XNULL(this)
  return this->doc;
}

ApackDoc *mdoc_functions(Mdoc *this) {
  XNULL(this)
  return this->functions;
}

Aclass *mdoc_classes(Mdoc *this) {
  XNULL(this)
  return this->classes;
}

Json *mdoc_to_json(Mdoc *this) {
  XNULL(this)
  Ajson *serial = ajson_new();
  jarr_astring(serial, this->doc);
  jarr_aarray(serial, (Arr *)this->functions, (Json*(*)(void*)) packDoc_to_json);
  jarr_aarray(serial, (Arr *)this->classes, (Json*(*)(void*)) class_to_json);
  return json_warray(serial);
}

Mdoc *mdoc_from_json(Json *js) {
  XNULL(js)
  Ajson *serial = json_rarray(js);
  Mdoc *this = MALLOC(Mdoc);
  size_t i = 0;
  this->doc = jarr_gstring(serial, i++);
  this->functions = (ApackDoc *)jarr_garray(serial, i++, (void*(*)(Json*)) packDoc_from_json);
  this->classes = (Aclass *)jarr_garray(serial, i++, (void*(*)(Json*)) class_from_json);
  return this;
}
/*.-.*/
