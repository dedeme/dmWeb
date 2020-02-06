// Copyright 12-Jan-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Rgex.h"
/* .
Rgex: serial
  rgex: char *
  subs: char *
*/

/*--*/

struct Rgex_Rgex {
  char *rgex;
  char *subs;
};

Rgex *rgex_new (char *rgex, char *subs) {
  Rgex *this = MALLOC(Rgex);
  this->rgex = rgex;
  this->subs = subs;
  return this;
}

char *rgex_rgex (Rgex *this) {
  return this->rgex;
}

char *rgex_subs (Rgex *this) {
  return this->subs;
}

Js *rgex_to_js (Rgex *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(this->rgex));
  arr_push(js, js_ws(this->subs));
  return js_wa(js);
}

Rgex *rgex_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  Rgex *this = MALLOC(Rgex);
  this->rgex = js_rs(*p++);
  this->subs = js_rs(*p++);
  return this;
}

/*--*/

