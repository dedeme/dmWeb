// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/dfleas/mmWin/MMWinBase.h"

MMWinBase *mMWinBase_new(int to_sell, double ref, double max) {
  MMWinBase *this = MALLOC(MMWinBase);
  this->to_sell = to_sell;
  this->ref = ref;
  this->mm = max;
  this->ref0 = max;
  this->qop = max;
  this->qlast = max;
  return this;
}

/// Returns Arr[MMWinBase]
Arr *mMWinBase_cos (int qnicks, QmatrixValues *closes) {
  Arr *r = arr_new();
  QmatrixValues *p;
  RANGE0(nk, qnicks)
    p = closes;
    double q = (*p++)[nk];
    while (q < 0) {
      q = (*p++)[nk];
    }
    arr_push(r, mMWinBase_new(1, q * (0.95), q));
  _RANGE
  return r;
}

Order *mMWinBase_order (
  MMWinBase *this, double q, double step_to_buy, double step_to_sell
) {
  this->qlast = q;
  if (q > 0) {
    if (this->to_sell) {
      if (q > this->ref) {
        this->ref = this->ref + (q - this->ref) * step_to_sell;
      }
      if (q <= this->ref && (q > this->qop || q < this->ref0)) {
        this->ref = this->mm;
        this->ref0 = this->mm;
        this->mm = q;
        this->qop = q;
        this->to_sell = 0;
        return order_sell();
      } else {
        this->mm = q > this->mm ? q : this->mm;
        return order_none();
      }
    } else {
      if (q < this->ref) {
        this->ref = this->ref - (this->ref - q) * step_to_buy;
      }
      if (q >= this->ref && (q < this->qop || q > this->ref0)) {
        double pond = (q - this->ref) / this->ref;
        this->ref = this->mm;
        this->ref0 = this->mm;
        this->mm = q;
        this->qop = q;
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

double mMWinBase_ref (MMWinBase *this) {
  return
      (this->to_sell && this->ref > this->qop && this->ref < this->qlast) ||
      (!this->to_sell && this->ref < this->qop && this->ref > this->qlast)
    ? this->ref
    : this->ref0
  ;
}
