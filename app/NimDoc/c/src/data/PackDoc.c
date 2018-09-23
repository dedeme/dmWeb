// Copyright 06-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/PackDoc.h"
/*.
struct: PackDoc
  id:char *: _string
  definition: char *: _string
  doc: char *: _string
*/
/*.-.*/
#include "dmc/ct/Ajson.h"

struct packDoc_PackDoc {
  char *id;
  char *definition;
  char *doc;
};

PackDoc *packDoc_new(char *id, char *definition, char *doc) {
  PackDoc *this = MALLOC(PackDoc);
  XNULL(id)
  this->id = id;
  XNULL(definition)
  this->definition = definition;
  XNULL(doc)
  this->doc = doc;
  return this;
}

char *packDoc_id(PackDoc *this) {
  XNULL(this)
  return this->id;
}

char *packDoc_definition(PackDoc *this) {
  XNULL(this)
  return this->definition;
}

char *packDoc_doc(PackDoc *this) {
  XNULL(this)
  return this->doc;
}

Json *packDoc_to_json(PackDoc *this) {
  XNULL(this)
  Ajson *serial = ajson_new();
  jarr_astring(serial, this->id);
  jarr_astring(serial, this->definition);
  jarr_astring(serial, this->doc);
  return json_warray(serial);
}

PackDoc *packDoc_from_json(Json *js) {
  XNULL(js)
  Ajson *serial = json_rarray(js);
  PackDoc *this = MALLOC(PackDoc);
  size_t i = 0;
  this->id = jarr_gstring(serial, i++);
  this->definition = jarr_gstring(serial, i++);
  this->doc = jarr_gstring(serial, i++);
  return this;
}
/*.-.*/

#define TY PackDoc
#define FN packDoc
#include "dmc/tpl/tarr.c"
#include "dmc/tpl/topt.c"
#undef TY
#undef FN
