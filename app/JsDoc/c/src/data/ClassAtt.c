// Copyright 07-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/ClassAtt.h"
/*.
struct: ClassAtt
  id: char *: _string
  +type: char *: _string
  +get_doc: PackDoc *: packDoc
  +set_doc: PackDoc *: packDoc
*/
/*.-.*/
#include "dmc/ct/Ajson.h"

struct classAtt_ClassAtt {
  char *id;
  char *type;
  PackDoc *get_doc;
  PackDoc *set_doc;
};

ClassAtt *classAtt_new(
  char *id,
  char *type,
  PackDoc *get_doc,
  PackDoc *set_doc
) {
  ClassAtt *this = MALLOC(ClassAtt);
  XNULL(id)
  this->id = id;
  XNULL(type)
  this->type = type;
  XNULL(get_doc)
  this->get_doc = get_doc;
  XNULL(set_doc)
  this->set_doc = set_doc;
  return this;
}

char *classAtt_id(ClassAtt *this) {
  XNULL(this)
  return this->id;
}

char *classAtt_type(ClassAtt *this) {
  XNULL(this)
  return this->type;
}

void classAtt_set_type(ClassAtt *this, char *value) {
  XNULL(this)
  XNULL(value)
  this->type = value;
}

PackDoc *classAtt_get_doc(ClassAtt *this) {
  XNULL(this)
  return this->get_doc;
}

void classAtt_set_get_doc(ClassAtt *this, PackDoc *value) {
  XNULL(this)
  XNULL(value)
  this->get_doc = value;
}

PackDoc *classAtt_set_doc(ClassAtt *this) {
  XNULL(this)
  return this->set_doc;
}

void classAtt_set_set_doc(ClassAtt *this, PackDoc *value) {
  XNULL(this)
  XNULL(value)
  this->set_doc = value;
}

Json *classAtt_to_json(ClassAtt *this) {
  XNULL(this)
  Ajson *serial = ajson_new();
  jarr_astring(serial, this->id);
  jarr_astring(serial, this->type);
  ajson_add(serial, packDoc_to_json(this->get_doc));
  ajson_add(serial, packDoc_to_json(this->set_doc));
  return json_warray(serial);
}

ClassAtt *classAtt_from_json(Json *js) {
  XNULL(js)
  Ajson *serial = json_rarray(js);
  ClassAtt *this = MALLOC(ClassAtt);
  size_t i = 0;
  this->id = jarr_gstring(serial, i++);
  this->type = jarr_gstring(serial, i++);
  this->get_doc = packDoc_from_json(ajson_get(serial, i++));
  this->set_doc = packDoc_from_json(ajson_get(serial, i++));
  return this;
}
/*.-.*/

#define TY ClassAtt
#define FN classAtt
#include "dmc/tpl/tarr.c"
#include "dmc/tpl/tit.c"
#include "dmc/tpl/topt.c"
#undef TY
#undef FN

