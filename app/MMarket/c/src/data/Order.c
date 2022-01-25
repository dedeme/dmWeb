// Copyright 22-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Order.h"
#include "dmc/js.h"
#include "dmc/DEFS.h"

Order *order_new (
  char *date, char *nick, int is_sell, int stocks, double price
) {
  Order *this = MALLOC(Order);
  this->date = date;
  this->nick = nick;
  this->is_sell = is_sell;
  this->stocks = stocks;
  this->price = price;
  return this;
}

///
char *order_to_js (Order *this) {
  return js_wa(achar_new_from(
    js_ws(this->date),
    js_ws(this->nick),
    js_wb(this->is_sell),
    js_wi(this->stocks),
    js_wd(this->price),
    NULL
  ));
}

///
Order *order_from_js (char *js) {
  Achar *a = js_ra(js);
  return order_new(
    js_rs(achar_get(a, 0)),
    js_rs(achar_get(a, 1)),
    js_rb(achar_get(a, 2)),
    js_ri(achar_get(a, 3)),
    js_rd(achar_get(a, 4))
  );
}

