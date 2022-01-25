// Copyright 12-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Conf.h"
#include "dmc/DEFS.h"
#include "dmc/js.h"
#include "dmc/char/Achar.h"

Conf *conf_new (char *lang) {
  Conf *this = MALLOC(Conf);
  this->lang = lang;
  return this;
}

char *conf_to_js (Conf *this) {
  return js_wa(achar_new_from(
    js_ws(this->lang),
    NULL
  ));
}

Conf *conf_from_js (char *js) {
  char **p = js_ra(js)->es;
  Conf *this = MALLOC(Conf);
  this->lang = js_rs(*p++);
  return this;
}
