// Copyright 14-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Facc.h"
#include "DEFS.h"
#include "data/broker.h"

struct Facc_Facc{
  double cash;
  int *pf;
};

Facc *facc_new(int ncos) {
  Facc *this = MALLOC(Facc);
  this->cash = INITIAL_CAPITAL;
  int *pf = ATOMIC(ncos * sizeof(int));
  int *p = pf;
  REPEAT(ncos)
    *p++ = 0;
  _REPEAT
  this->pf = pf;
  return this;
}

double facc_cash(Facc *this) {
  return this->cash;
}

int *facc_pf(Facc *this) {
  return this->pf;
}

int facc_buy(Facc *this, int co, double price) {
  int stocks = (int)(BET / price);
  double money = broker_buy(stocks, price);
  if (money < this->cash) {
    this->pf[co] += stocks;
    this->cash -= money;
    return 1;
  }
  return 0;
}

int facc_sell(Facc *this, int co, double price) {
  int stocks = this->pf[co];
  if (stocks) {
    this->pf[co] = 0;
    this->cash += broker_sell(stocks, price);
    return 1;
  }
  return 0;
}

double facc_assets(
  Facc *this,
  int ncos,
  Qmatrix *closes,
  int ix
) {
  double r = this->cash;
  QmatrixValues *cs = qmatrix_values(closes);
  RANGE0(i, ncos)
    int stocks = this->pf[i];
    if (stocks) {
      int value = cs[ix][i];
      while (value < 0) {
        if (ix == 0) {
          value = 0;
          break;
        }
        --ix;
        value = cs[ix][i];
      }
      r += broker_sell(stocks, value);
    }
  _RANGE
  return r;
}
