// Copyright 18-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Dpath.h"

/* .
# Documentation path data.
Dpath: serial
  id: char *
  path: char *
  @show: bool
  @valid: bool
*/
/*--*/

struct Dpath_Dpath {
  char *id;
  char *path;
  int show;
  int valid;
};

Dpath *dpath_new (
  char *id,
  char *path,
  int show,
  int valid
) {
  Dpath *this = MALLOC(Dpath);
  this->id = id;
  this->path = path;
  this->show = show;
  this->valid = valid;
  return this;
}

char *dpath_id (Dpath *this) {
  return this->id;
}

char *dpath_path (Dpath *this) {
  return this->path;
}

int dpath_show (Dpath *this) {
  return this->show;
}

void dpath_set_show (Dpath *this, int value) {
  this->show = value;
}

int dpath_valid (Dpath *this) {
  return this->valid;
}

void dpath_set_valid (Dpath *this, int value) {
  this->valid = value;
}

Js *dpath_to_js (Dpath *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(this->id));
  arr_push(js, js_ws(this->path));
  arr_push(js, js_wb(this->show));
  arr_push(js, js_wb(this->valid));
  return js_wa(js);
}

Dpath *dpath_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  Dpath *this = MALLOC(Dpath);
  this->id = js_rs(*p++);
  this->path = js_rs(*p++);
  this->show = js_rb(*p++);
  this->valid = js_rb(*p++);
  return this;
}

/*--*/
