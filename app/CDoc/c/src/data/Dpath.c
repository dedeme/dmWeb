// Copyright 08-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Dpath.h"
#include "dmc/DEFS.h"
#include "dmc/js.h"
#include "dmc/char/Achar.h"

Dpath *dpath_new (
  char *id,
  char *path,
  int is_shown,
  int is_valid
) {
  Dpath *this = MALLOC(Dpath);
  this->id = id;
  this->path = path;
  this->is_shown = is_shown;
  this->is_valid = is_valid;
  return this;
}

void dpath_set_shown (Dpath *this, int value) {
  this->is_shown = value;
}

void dpath_set_valid (Dpath *this, int value) {
  this->is_valid = value;
}

char *dpath_to_js (Dpath *this) {
  return js_wa(achar_new_from(
    js_ws(this->id),
    js_ws(this->path),
    js_wb(this->is_shown),
    js_wb(this->is_valid),
    NULL
  ));
}

Dpath *dpath_from_js (char *js) {
  char **p = js_ra(js)->es;
  Dpath *this = MALLOC(Dpath);
  this->id = js_rs(*p++);
  this->path = js_rs(*p++);
  this->is_shown = js_rb(*p++);
  this->is_valid = js_rb(*p++);
  return this;
}
