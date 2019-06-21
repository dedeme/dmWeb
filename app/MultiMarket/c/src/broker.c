// Copyright 16-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "broker.h"

double broker_fees (double money) {
  double broker = 9.75;
  if (money > 25000) {
    broker = money * 0.001;
  }

  double bolsa = 4.65 + money * 0.00012;
  if (money > 140000) {
    bolsa = 13.4;
  } else if (money > 70000) {
    bolsa = 9.2 + money * 0.00003;
  } else if (money > 35000) {
    bolsa = 6.4 + money * 0.00007;
  }

  return broker + bolsa;
}

double broker_buy (int stocks, double price) {
  double money = stocks * price;
  return money + broker_fees(money);
}

double broker_sell (int stocks, double price) {
  double money = stocks * price;
  return money - broker_fees(money);
}
