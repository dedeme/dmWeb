// Copyright 16-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "order_tests.h"
#include "assert.h"
#include "data/Order.h"

void order_tests() {
  puts("Order tests:");

  OrderCos *os = orderCos_new(105);
  orderCos_addOrder(os, 0, order_none());
  orderCos_addOrder(os, 1, order_buy(1.2));
  orderCos_addOrder(os, 2, order_sell());
  orderCos_addOrder(os, 3, order_none());
  orderCos_addOrder(os, 4, order_none());
  orderCos_addOrder(os, 5, order_sell());
  orderCos_addOrder(os, 6, order_buy(1.4));
  orderCos_addOrder(os, 7, order_none());
  orderCos_addOrder(os, 8, order_buy(1.1));

  assert(orderCos_nsells(os) == 2);
  assert(orderCos_nbuys(os) == 3);

  assert(orderCos_sells(os)[0] == 2);
  assert(orderCos_sells(os)[1] == 5);

  assert(orderCos_buys(os)[0] == 6);
  assert(orderCos_buys(os)[1] == 1);
  assert(orderCos_buys(os)[2] == 8);

  puts("    Finished");
}



