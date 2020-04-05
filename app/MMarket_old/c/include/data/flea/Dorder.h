// Copyright 24-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Daily order.

#ifndef DATA_FLEA_DORDER_H
  #define DATA_FLEA_DORDER_H

#include "dmc/async.h"

/*--*/

///
///   Arguments:
///     is_sell: bool
///     co_ix: int
///     pond: double
typedef struct Dorder_Dorder Dorder;

///
int dorder_is_sell (Dorder *this);

///
int dorder_co_ix (Dorder *this);

///
double dorder_pond (Dorder *this);

/*--*/

/// Creates a sell order
///   co_ix: Company index in a QtableRow.
Dorder *dorder_sell (int co_ix);

/// Creates a buy order
///   co_ix: Company index in a QtableRow.
///   pond : Priority of order.
Dorder *dorder_buy (int co_ix, double pond);

// 'dorders' is Arr[Dorder]
void dorder_sort (Arr *dorders);

#endif
