// Copyright 18-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/broker.h"

double broker_fees(double amount) {
  amount += amount * 0.0017 + 9.53; // penalty for operating.
  double brk = (amount > 50000) ? amount * 0.001 : 9.75;
	double market = amount * 0.00003 + 0.11; // market fee + market execution
  return brk + market;
}

double broker_buy(int stocks, double price) {
  double amount = stocks * price;
	double tobin = amount * 0.002;
  return amount + broker_fees(amount) + tobin;
}

double broker_sell(int stocks, double price) {
  double amount = stocks * price;
  return amount - broker_fees(amount);
}
