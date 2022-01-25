// Copyright 18-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Broker operations.

#ifndef DATA_BROKER_H
  #define DATA_BROKER_H

/// Returns total fees of a buy or sell operation.
///   amount: Operation amount.
double broker_fees(double amount);

/// Returns total cost of operation (cost + fees).
///   stocks: Stocks number.
///   price  ; Stocks price.
double broker_buy(int stocks, double price);

/// Returns net incomes of operation (incomes - fees).
///   stocks: Stocks number.
///   price  ; Stocks price.
double broker_sell(int stocks, double price);

#endif
