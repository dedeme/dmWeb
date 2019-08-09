// Copyright 01-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/dfleas/incr/IncrBase.h"

IncrBase *incrBase_new(
  int to_sell, QmatrixValues *closes, double ref, int con
) {
  IncrBase *this = MALLOC(IncrBase);
  this->to_sell = to_sell;
  this->closes = closes;
  this->ref = ref;
  this->con = con;
  this->closes_ix = 0;
  this->days_ix = 0;
  return this;
}

/// Returns Arr[IncrBase]
Arr *incrBase_cos (int qnicks, QmatrixValues *closes, double strip_to_sell) {
    Arr *r = arr_new();
  RANGE0(i, qnicks)
    arr_push(r, incrBase_new(1, closes, closes[0][i] * (1 - strip_to_sell), i));
  _RANGE
  return r;
}

Order *incrBase_order (
  IncrBase *this, double q, int days, double strip_to_buy, double strip_to_sell
) {
  if (this->days_ix < days) {
    this->days_ix += 1;
    return order_none();
  }

  ++this->closes_ix;
  double new_ref = this->closes[this->closes_ix][this->con];
  if (new_ref < 0) {
    new_ref = this->ref;
  }
  if (q < 0 || new_ref < 0) {
    return order_none();
  }

  if (this->to_sell) {
    if (new_ref > this->ref) {
      this->ref = new_ref;
    }
    if (q <= (this->ref * (1 - strip_to_sell))) {
      this->ref = new_ref;
      this->to_sell = 0;
      return order_sell();
    } else {
      return order_none();
    }
  } else {
    if (new_ref < this->ref) {
      this->ref = new_ref;
    }
    double ref = this->ref * (1 + strip_to_buy);
    if (q >= ref) {
      double pond = (q - ref) / ref;
      this->ref = new_ref;
      this->to_sell = 1;
      return order_buy(pond);
    } else {
      return order_none();
    }
  }
}

double incrBase_ref (
  IncrBase *this, double strip_to_buy, double strip_to_sell
) {
  return this->to_sell
    ? this->ref * (1 - strip_to_sell)
    : this->ref * (1 + strip_to_buy)
  ;
}
