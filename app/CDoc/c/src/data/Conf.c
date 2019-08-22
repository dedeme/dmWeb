// Copyright 18-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Conf.h"

/* .
# Configuration data.
Conf: serial
  path: char *
  lang: char *
  show_all: bool
*/

/*--*/

struct Conf_Conf {
  char *path;
  char *lang;
  int show_all;
};

Conf *conf_new (char *path, char *lang, int show_all) {
  Conf *this = MALLOC(Conf);
  this->path = path;
  this->lang = lang;
  this->show_all = show_all;
  return this;
}

char *conf_path (Conf *this) {
  return this->path;
}

char *conf_lang (Conf *this) {
  return this->lang;
}

int conf_show_all (Conf *this) {
  return this->show_all;
}

Js *conf_to_js (Conf *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(this->path));
  arr_push(js, js_ws(this->lang));
  arr_push(js, js_wb(this->show_all));
  return js_wa(js);
}

Conf *conf_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  Conf *this = MALLOC(Conf);
  this->path = js_rs(*p++);
  this->lang = js_rs(*p++);
  this->show_all = js_rb(*p++);
  return this;
}

/*--*/
