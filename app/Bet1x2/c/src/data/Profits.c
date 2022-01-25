// Copyright 29-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Profits.h"
#include "dmc/js.h"
#include "dmc/DEFS.h"

Profits *profits_new (char *description, int hits, int fails, double amount) {
  Profits *this = MALLOC(Profits);
  this->description = description;
  this->hits = hits;
  this->fails = fails;
  this->amount = amount;
  return this;
}

char *profits_to_js (Profits *this) {
  return js_wa(achar_new_from(
    js_ws(this->description),
    js_wi(this->hits),
    js_wi(this->fails),
    js_wd(this->amount),
    NULL
  ));
}

Profits *profits_from_js(char *js) {
  Achar *a = js_ra(js);
  return profits_new(
    js_rs(achar_get(a, 0)),
    js_ri(achar_get(a, 1)),
    js_ri(achar_get(a, 2)),
    js_rd(achar_get(a, 3))
  );
}
