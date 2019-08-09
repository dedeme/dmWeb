// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/dfleas/mmBack/MMBackBase.h"

MMBackBase *mMBackBase_new(int to_sell, double ref, double q) {
  MMBackBase *this = MALLOC(MMBackBase);
  this->to_sell = to_sell;
  this->ref = ref;
  this->mm = q;
  this->pmm1 = q;
  this->pmm2 = q;
  this->q = q;
  return this;
}

/// Returns Arr[MMBackBase]
Arr *mMBackBase_cos (int qnicks, QmatrixValues *closes) {
  Arr *r = arr_new();
  QmatrixValues *p;
  RANGE0(nk, qnicks)
    p = closes;
    double q = (*p++)[nk];
    while (q < 0) {
      q = (*p++)[nk];
    }
    arr_push(r, mMBackBase_new(1, q * (0.95), q));
  _RANGE
  return r;
}

Order *mMBackBase_order (
  MMBackBase *this, double q, double step_to_buy, double step_to_sell
) {
  if (q > 0) {
    if (this->to_sell) {
      this->ref = this->ref + (q - this->ref) * step_to_sell;
      if (q <= this->ref) {
        this->ref = q > this->q || this->mm > this->pmm2
          ? this->mm
          : this->pmm2;
        this->pmm2 = this->pmm1;
        this->pmm1 = this->mm;
        this->mm = q;
        this->q = q;
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
        this->ref = q < this->q || this->mm < this->pmm2
          ? this->mm
          : this->pmm2;
        this->pmm2 = this->pmm1;
        this->pmm1 = this->mm;
        this->mm = q;
        this->q = q;
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
