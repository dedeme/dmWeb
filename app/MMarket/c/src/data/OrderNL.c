// Copyright 29-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/OrderNL.h"
#include "dmc/js.h"
#include "dmc/DEFS.h"

OrderNL *orderNL_new (
  char *date, char *nick, int type, int stocks, double price
) {
  OrderNL *this = MALLOC(OrderNL);
  this->date = date;
  this->nick = nick;
  this->type = type;
  this->stocks = stocks;
  this->price = price;
  return this;
}

char *orderNL_to_js (OrderNL *this) {
  return js_wa(achar_new_from(
    js_ws(this->date),
    js_ws(this->nick),
    js_wi(this->type),
    js_wi(this->stocks),
    js_wd(this->price),
    NULL
  ));
}

OrderNL *orderNL_from_js (char *js) {
  Achar *a = js_ra(js);
  return orderNL_new(
    js_rs(achar_get(a, 0)),
    js_rs(achar_get(a, 1)),
    js_rb(achar_get(a, 2)),
    js_ri(achar_get(a, 3)),
    js_rd(achar_get(a, 4))
  );
}

