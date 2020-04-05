// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>
/// Broker operations.

#ifndef DATA_BROKER_H
  #define DATA_BROKER_H

#include "dmc/async.h"

/// Returns tatal fees of a buy or sell operation.
double broker_fees (double amount);

/// Returns net cost of operation
double broker_buy (int stocks, double price);

/// Returns net incomes of operation
double broker_sell (int stocks, double price);

#endif
