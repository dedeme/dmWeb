// Copyright 06-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/MenuPath.h"

/*.
struct: MenuPath
  +id : char *: _string
  +path : char *: _string
  +show : bool: _bool
  +ok: bool: _bool
*/
/*.-.*/
#include "dmc/ct/Ajson.h"

struct menuPath_MenuPath {
  char *id;
  char *path;
  bool show;
  bool ok;
};

MenuPath *menuPath_new(
  char *id,
  char *path,
  bool show,
  bool ok
) {
  MenuPath *this = MALLOC(MenuPath);
  XNULL(id)
  this->id = id;
  XNULL(path)
  this->path = path;
  this->show = show;
  this->ok = ok;
  return this;
}

char *menuPath_id(MenuPath *this) {
  XNULL(this)
  return this->id;
}

void menuPath_set_id(MenuPath *this, char *value) {
  XNULL(this)
  XNULL(value)
  this->id = value;
}

char *menuPath_path(MenuPath *this) {
  XNULL(this)
  return this->path;
}

void menuPath_set_path(MenuPath *this, char *value) {
  XNULL(this)
  XNULL(value)
  this->path = value;
}

bool menuPath_show(MenuPath *this) {
  XNULL(this)
  return this->show;
}

void menuPath_set_show(MenuPath *this, bool value) {
  XNULL(this)
  this->show = value;
}

bool menuPath_ok(MenuPath *this) {
  XNULL(this)
  return this->ok;
}

void menuPath_set_ok(MenuPath *this, bool value) {
  XNULL(this)
  this->ok = value;
}

Json *menuPath_to_json(MenuPath *this) {
  XNULL(this)
  Ajson *serial = ajson_new();
  jarr_astring(serial, this->id);
  jarr_astring(serial, this->path);
  jarr_abool(serial, this->show);
  jarr_abool(serial, this->ok);
  return json_warray(serial);
}

MenuPath *menuPath_from_json(Json *js) {
  XNULL(js)
  Ajson *serial = json_rarray(js);
  MenuPath *this = MALLOC(MenuPath);
  size_t i = 0;
  this->id = jarr_gstring(serial, i++);
  this->path = jarr_gstring(serial, i++);
  this->show = jarr_gbool(serial, i++);
  this->ok = jarr_gbool(serial, i++);
  return this;
}
/*.-.*/

#define TY MenuPath                    // Element type
#define FN menuPath                    // Function prefix
#include "dmc/tpl/tarr.c"
#undef TY
#undef FN

Json *amenuPath_to_json(AmenuPath *this) {
  return amenuPath_to_jsonf(this, menuPath_to_json);
}

AmenuPath *amenuPath_from_json(Json *js) {
  return amenuPath_from_jsonf(js, menuPath_from_json);
}
