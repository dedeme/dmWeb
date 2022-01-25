// Copyright 22-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Order send in Operations page.

#ifndef DATA_ORDER_H
  #define DATA_ORDER_H

/// Order send in Operations page.
struct order_Order {
  // Order date.
  char *date;
  // Company nick.
  char *nick;
  // '1' if operation is sale.
  int is_sell;
  // Stocks number.
  int stocks;
  // Price for stock.
  double price;
};

/// Order send in Operations page.
typedef struct order_Order Order;

///
Order *order_new (
  char *date, char *nick, int is_sell, int stocks, double price
);

///
char *order_to_js (Order *this);

///
Order *order_from_js (char *);

#endif
