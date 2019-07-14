// Copyright 11-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Order.h"

/* .
-Order
  # Values are 0: no operation, 1: buy, 2: sell
  -type: int
  # Returns ponderation of a buy order.
  ponderation: double
*/
/*--*/

struct Order_Order {
  int type;
  double ponderation;
};

static Order *_order_new (int type, double ponderation) {
  Order *this = MALLOC(Order);
  this->type = type;
  this->ponderation = ponderation;
  return this;
}

double order_ponderation (Order *this) {
  return this->ponderation;
}

/*--*/

Order *order_none (void) {
  return _order_new(0, -1);
}

Order *order_buy (double ponderation) {
  return _order_new(1, ponderation);
}

Order *order_sell (void) {
  return _order_new(2, -1);
}

int order_is_none (Order *this) {
  return !this->type;
}

int order_is_buy (Order *this) {
  return this->type == 1;
}

int order_is_sell (Order *this) {
  return this->type == 2;
}

struct Order_OrderCos{
  int *buys;
  double *ponderations;
  int nbuys;
  int *sells;
  int nsells;
};

OrderCos *orderCos_new(int ncos) {
  OrderCos *this = MALLOC(OrderCos);
  this->buys = ATOMIC(ncos * sizeof(int));
  this->ponderations = ATOMIC(ncos * sizeof(double));
  this->nbuys = 0;
  this->sells = ATOMIC(ncos * sizeof(int));
  this->nsells = 0;
  return this;
}

int orderCos_nbuys(OrderCos *this) {
  return this->nbuys;
}

int *orderCos_buys(OrderCos *this) {
  return this->buys;
}

int orderCos_nsells(OrderCos *this) {
  return this->nsells;
}

int *orderCos_sells(OrderCos *this) {
  return this->sells;
}

void orderCos_add(OrderCos *this, int co, Order *o) {
  if (o->type == 1) {
    double pon = o->ponderation;
    int i;
    for (i = 0; i < this->nbuys; ++i) {
      if (this->ponderations[i] < pon) {
        for (; i < this->nbuys; ++i) {
          int co_tmp = this->buys[i];
          double pon_tmp = this->ponderations[i];
          this->buys[i] = co;
          this->ponderations[i] = pon;
          co = co_tmp;
          pon = pon_tmp;
        }
        break;
      }
    }
    this->buys[i] = co;
    this->ponderations[i] = pon;
    ++this->nbuys;
  } else if (o->type == 2) {
    this->sells[this->nsells] = co;
    ++this->nsells;
  }
}

