// Copyright 29-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Order send in NoLost page.

#ifndef DATA_ORDERNL_H
  #define DATA_ORDERNL_H

/// Order type
enum {orderNL_BUY, orderNL_SELL, orderNL_CATCH};

/// Order send in NoLost page.
struct orderNL_OrderNL {
  // Order date.
  char *date;
  // Company nick.
  char *nick;
  // orderNL_BUY, orderNL_SELL, orderNL_CATCH.
  int type;
  // Stocks number.
  int stocks;
  // Price for stock.
  double price;
};

/// Order send in Operations page.
typedef struct orderNL_OrderNL OrderNL;

///
OrderNL *orderNL_new (
  char *date, char *nick, int type, int stocks, double price
);

///
char *orderNL_to_js (OrderNL *this);

///
OrderNL *orderNL_from_js (char *);

#endif
