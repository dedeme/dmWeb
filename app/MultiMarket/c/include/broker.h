// Copyright 16-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Broker operations.

#ifndef BROKER_H
  #define BROKER_H

#include "dmc/std.h"

/// Returns tatal fees of a buy or sell operation.
double broker_fees (double money);

/// Returns net cost of operation
double broker_buy (int stocks, double price);

/// Returns net incomes of operation
double broker_sell (int stocks, double price);

#endif
