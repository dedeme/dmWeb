// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/dfleas/mm/MMBase.h"

MMBase *mMBase_new(int to_sell, double ref, double q) {
  MMBase *this = MALLOC(MMBase);
  this->to_sell = to_sell;
  this->ref = ref;
  this->mm = q;
  return this;
}

// Returns Arr[ApproxBase]
Arr *mMBase_cos (int qnicks, QmatrixValues *closes, double strip_to_sell) {
  Arr *r = arr_new();
  QmatrixValues *p;
  RANGE0(nk, qnicks)
    p = closes;
    double q = (*p++)[nk];
    while (q < 0) {
      q = (*p++)[nk];
    }
    arr_push(
      r,
      mMBase_new(1, q * (0.95) * (1 - strip_to_sell) , q)
    );
  _RANGE
  return r;
}

Order *mMBase_order (
  MMBase *this, double q,
  double strip_to_buy, double step_to_buy,
  double strip_to_sell, double step_to_sell
) {
  if (q > 0) {
    if (this->to_sell) {
      this->ref = this->ref + (q - this->ref) * step_to_sell;
      if (q <= this->ref) {
        this->ref = this->mm * (1 + strip_to_buy);
        this->mm = q;
        this->to_sell = 0;
        return order_sell();
      } else {
        this->mm = q > this->mm ? q : this->mm;
        return order_none();
      }
    } else {
      this->ref = this->ref - (this->ref - q) * step_to_buy;
      if (q >= this->ref) {
        double pond = (q - this->ref) / this->ref;
        this->ref = this->mm * (1 - strip_to_sell);
        this->mm = q;
        this->to_sell = 1;
        return order_buy(pond);
      } else {
        this->mm = q < this->mm ? q : this->mm;
        return order_none();
      }
    }
  } else {
    return order_none();
  }
}

