// Copyright 06-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Nick.h"

/* .
Nick: serial
  id: int
  @name: char *
  ---
  @ is_sel: bool: 0
*/

/*--*/

struct Nick_Nick{
  int id;
  char *name;
  int is_sel;
};

Nick *nick_new(int id, char *name) {
  Nick *this = MALLOC(Nick);
  this->id = id;
  this->name = name;
  this->is_sel = 0;
  return this;
}

int nick_id(Nick *this) {
  return this->id;
}

char *nick_name(Nick *this) {
  return this->name;
}

void nick_set_name(Nick *this, char *value) {
  this->name = value;
}

int nick_is_sel(Nick *this) {
  return this->is_sel;
}

void nick_set_is_sel(Nick *this, int value) {
  this->is_sel = value;
}

Js *nick_to_js(Nick *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_wi((int)this->id));
  arr_push(js, js_ws(this->name));
  arr_push(js, js_wb(this->is_sel));
  return js_wa(js);
}

Nick *nick_from_js(Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  Nick *this = MALLOC(Nick);
  this->id = js_ri(*p++);
  this->name = js_rs(*p++);
  this->is_sel = js_rb(*p++);
  return this;
}

/*--*/
