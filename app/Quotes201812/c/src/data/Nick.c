// Copyright 11-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Nick.h"
/* .+.
struct: Nick
  id: char *: _string
  +name: char *: _string
  +is_ibex: bool: _bool
  +is_sel: bool: _bool
*/
/*.-.*/
#include "dmc/ct/Ajson.h"

struct nick_Nick {
  char *id;
  char *name;
  bool is_ibex;
  bool is_sel;
};

Nick *nick_new(
  char *id,
  char *name,
  bool is_ibex,
  bool is_sel
) {
  Nick *this = MALLOC(Nick);
  XNULL(id)
  this->id = id;
  XNULL(name)
  this->name = name;
  this->is_ibex = is_ibex;
  this->is_sel = is_sel;
  return this;
}

char *nick_id(Nick *this) {
  XNULL(this)
  return this->id;
}

char *nick_name(Nick *this) {
  XNULL(this)
  return this->name;
}

void nick_set_name(Nick *this, char *value) {
  XNULL(this)
  XNULL(value)
  this->name = value;
}

bool nick_is_ibex(Nick *this) {
  XNULL(this)
  return this->is_ibex;
}

void nick_set_is_ibex(Nick *this, bool value) {
  XNULL(this)
  this->is_ibex = value;
}

bool nick_is_sel(Nick *this) {
  XNULL(this)
  return this->is_sel;
}

void nick_set_is_sel(Nick *this, bool value) {
  XNULL(this)
  this->is_sel = value;
}

Json *nick_to_json(Nick *this) {
  XNULL(this)
  Ajson *serial = ajson_new();
  jarr_astring(serial, this->id);
  jarr_astring(serial, this->name);
  jarr_abool(serial, this->is_ibex);
  jarr_abool(serial, this->is_sel);
  return json_warray(serial);
}

Nick *nick_from_json(Json *js) {
  XNULL(js)
  Ajson *serial = json_rarray(js);
  Nick *this = MALLOC(Nick);
  size_t i = 0;
  this->id = jarr_gstring(serial, i++);
  this->name = jarr_gstring(serial, i++);
  this->is_ibex = jarr_gbool(serial, i++);
  this->is_sel = jarr_gbool(serial, i++);
  return this;
}
/*.-.*/

#define TY Nick
#define FN nick
#include "dmc/tpl/tarr.c"
#undef TY
#undef FN

Json *anick_to_json(Anick *this) {
  return anick_to_jsonf(this, nick_to_json);
}

Anick *anick_from_json(Json *js) {
  return anick_from_jsonf(js, nick_from_json);
}
