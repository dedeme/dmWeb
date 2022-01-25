// Copyright 08-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Conf.h"
#include "dmc/DEFS.h"
#include "dmc/js.h"
#include "dmc/char/Achar.h"

Conf *conf_new (char *path, char *lang, int show_all) {
  Conf *this = MALLOC(Conf);
  this->path = path;
  this->lang = lang;
  this->show_all = show_all;
  return this;
}

char *conf_to_js (Conf *this) {
  return js_wa(achar_new_from(
    js_ws(this->path),
    js_ws(this->lang),
    js_wb(this->show_all),
    NULL
  ));
}

Conf *conf_from_js (char *js) {
  char **p = js_ra(js)->es;
  Conf *this = MALLOC(Conf);
  this->path = js_rs(*p++);
  this->lang = js_rs(*p++);
  this->show_all = js_rb(*p++);
  return this;
}
