// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/dfleas/approx/ApproxBase.h"

ApproxBase *approxBase_new(int to_sell, double ref) {
  ApproxBase *this = MALLOC(ApproxBase);
  this->to_sell = to_sell;
  this->ref = ref;
  return this;
}

// Returns Arr[ApproxBase]
Arr *approxBase_cos (int qnicks, QmatrixValues *closes, double start_to_sell) {
  Arr *r = arr_new();
  QmatrixValues *p;
  RANGE0(nk, qnicks)
    p = closes;
    double q = (*p++)[nk];
    while (q < 0) {
      q = (*p++)[nk];
    }
    arr_push(r, approxBase_new(1, q * (1 - start_to_sell)));
  _RANGE
  return r;
}

Order *approxBase_order (
  ApproxBase *this, double q,
  double start_to_buy, double step_to_buy,
  double start_to_sell, double step_to_sell
) {
  if (q > 0) {
    if (this->to_sell) {
      this->ref = this->ref + (q - this->ref) * step_to_sell;
      if (q <= this->ref) {
        this->ref = q * (1 + start_to_buy);
        this->to_sell = 0;
        return order_sell();
      } else {
        return order_none();
      }
    } else {
      this->ref = this->ref - (this->ref - q) * step_to_buy;
      if (q >= this->ref) {
        double pond = (q - this->ref) / this->ref;
        this->ref = q * (1 - start_to_sell);
        this->to_sell = 1;
        return order_buy(pond);
      } else {
        return order_none();
      }
    }
  } else {
    return order_none();
  }
}
