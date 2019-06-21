// Copyright 16-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "facc_tests.h"
#include "assert.h"
#include "dmc/Dec.h"
#include "DEFS.h"
#include "broker.h"
#include "data/Facc.h"

void facc_tests() {
  puts("Facc tests:");

  Facc *acc = facc_new(105);
  double cash = facc_cash(acc);
  int buy = 1;
  RANGE0(i, 12)
    if (buy) buy = facc_buy(acc, i, 1 + ((double)i) / 100);
  _RANGE

  int stocks[9];
  RANGE0(i, 105)
    double price = 1 + ((double)i) / 100;
    if (i < 9) {
      stocks[i] = (int)(BET / price);
      assert(stocks[i] == facc_pf(acc)[i]);
      cash -= broker_buy(stocks[i], price);
    } else {
      assert(facc_pf(acc)[i] == 0);
    }
  _RANGE

  assert(cash < BET);
  assert(dec_eq_gap(cash, facc_cash(acc), 0.001));
  assert(dec_eq_gap(cash, 14859.23, 0.001));

  facc_sell(acc, 3, 1);
  facc_sell(acc, 5, 1.5);

  cash += broker_sell(stocks[3], 1);
  cash += broker_sell(stocks[5], 1.5);

  assert(!facc_pf(acc)[3]);
  assert(!facc_pf(acc)[5]);

  assert(dec_eq_gap(cash, facc_cash(acc), 0.001));
  assert(dec_eq_gap(cash, 50816.611, 0.001));

  puts("    Finished");
}


