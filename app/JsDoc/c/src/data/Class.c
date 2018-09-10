// Copyright 07-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Class.h"
/*.
struct: Class
  id: char *: _string
  definition: char *: _string
  doc: char *: _string
  constructor: PackDoc *: packDoc
  methods: ApackDoc *: _array packDoc
  atts: AclassAtt *: _array classAtt
*/
/*.-.*/
#include "dmc/ct/Ajson.h"

struct class_Class {
  char *id;
  char *definition;
  char *doc;
  PackDoc *constructor;
  ApackDoc *methods;
  AclassAtt *atts;
};

Class *class_new(
  char *id,
  char *definition,
  char *doc,
  PackDoc *constructor,
  ApackDoc *methods,
  AclassAtt *atts
) {
  Class *this = MALLOC(Class);
  XNULL(id)
  this->id = id;
  XNULL(definition)
  this->definition = definition;
  XNULL(doc)
  this->doc = doc;
  XNULL(constructor)
  this->constructor = constructor;
  XNULL(methods)
  this->methods = methods;
  XNULL(atts)
  this->atts = atts;
  return this;
}

char *class_id(Class *this) {
  XNULL(this)
  return this->id;
}

char *class_definition(Class *this) {
  XNULL(this)
  return this->definition;
}

char *class_doc(Class *this) {
  XNULL(this)
  return this->doc;
}

PackDoc *class_constructor(Class *this) {
  XNULL(this)
  return this->constructor;
}

ApackDoc *class_methods(Class *this) {
  XNULL(this)
  return this->methods;
}

AclassAtt *class_atts(Class *this) {
  XNULL(this)
  return this->atts;
}

Json *class_to_json(Class *this) {
  XNULL(this)
  Ajson *serial = ajson_new();
  jarr_astring(serial, this->id);
  jarr_astring(serial, this->definition);
  jarr_astring(serial, this->doc);
  ajson_add(serial, packDoc_to_json(this->constructor));
  jarr_aarray(serial, (Arr *)this->methods, (Json*(*)(void*)) packDoc_to_json);
  jarr_aarray(serial, (Arr *)this->atts, (Json*(*)(void*)) classAtt_to_json);
  return json_warray(serial);
}

Class *class_from_json(Json *js) {
  XNULL(js)
  Ajson *serial = json_rarray(js);
  Class *this = MALLOC(Class);
  size_t i = 0;
  this->id = jarr_gstring(serial, i++);
  this->definition = jarr_gstring(serial, i++);
  this->doc = jarr_gstring(serial, i++);
  this->constructor = packDoc_from_json(ajson_get(serial, i++));
  this->methods = (ApackDoc *)jarr_garray(serial, i++, (void*(*)(Json*)) packDoc_from_json);
  this->atts = (AclassAtt *)jarr_garray(serial, i++, (void*(*)(Json*)) classAtt_from_json);
  return this;
}
/*.-.*/

#define TY Class
#define FN class
#include "dmc/tpl/tarr.c"
#undef TY
#undef FN
