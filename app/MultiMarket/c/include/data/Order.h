// Copyright 11-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Market order.

#ifndef DATA_ORDER_H
  #define DATA_ORDER_H

#include "dmc/std.h"

/*--*/

///
///   Arguments:
///     type: int
///     ponderation: double
typedef struct Order_Order Order;

/// Returns ponderation of a buy order.
double order_ponderation (Order *this);

/*--*/

/// Creates a order to do nothing.
Order *order_none (void);

/// Creates a buy order. 'ponderation' must be greater than 0.
Order *order_buy (double ponderation);

/// Creates a sell order
Order *order_sell (void);

///
int order_is_none (Order *this);

///
int order_is_buy (Order *this);

///
int order_is_sell (Order *this);

///
typedef struct Order_OrderCos OrderCos;

/// Daily orders. Initialized to 0 orders.
///   ncos: Total number of companies.
OrderCos *orderCos_new(int ncos);

/// Returns an array with companies to buy, sorted by ponderations. Its size
/// 'orderCos_nbuys'
int *orderCos_buys(OrderCos *this);

/// Number of companies to buy and ponderations
int orderCos_nbuys(OrderCos *this);

/// Returns an array with companies to sell. Its size is 'orderCos_nsells'
int *orderCos_sells(OrderCos *this);

/// Number of companies to sell
int orderCos_nsells(OrderCos *this);

///
void orderCos_add(OrderCos *this, int co, Order *o);

#endif
