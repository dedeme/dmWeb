// Copyright 08-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/MarketDay.h"

/* .
-MarketDay: SERIAL
  date: char *
  hopen: int
  mopen: int
  hclose: int
  mclose: int
*/

/*--*/

struct MarketDay_MarketDay{
  char *date;
  int hopen;
  int mopen;
  int hclose;
  int mclose;
};

char *marketDay_date(MarketDay *this) {
  return this->date;
}

int marketDay_hopen(MarketDay *this) {
  return this->hopen;
}

int marketDay_mopen(MarketDay *this) {
  return this->mopen;
}

int marketDay_hclose(MarketDay *this) {
  return this->hclose;
}

int marketDay_mclose(MarketDay *this) {
  return this->mclose;
}

Js *marketDay_to_js(MarketDay *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(this->date));
  arr_push(js, js_wi((int)this->hopen));
  arr_push(js, js_wi((int)this->mopen));
  arr_push(js, js_wi((int)this->hclose));
  arr_push(js, js_wi((int)this->mclose));
  return js_wa(js);
}

MarketDay *marketDay_from_js(Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  MarketDay *this = MALLOC(MarketDay);
  this->date = js_rs(*p++);
  this->hopen = js_ri(*p++);
  this->mopen = js_ri(*p++);
  this->hclose = js_ri(*p++);
  this->mclose = js_ri(*p++);
  return this;
}

/*--*/
