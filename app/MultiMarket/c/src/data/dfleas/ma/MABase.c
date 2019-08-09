// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/dfleas/ma/MABase.h"

MABase *mABase_new(QmatrixValues *closes, int con, int days) {
  MABase *this = MALLOC(MABase);
  this->to_sell = 1;
  this->closes = closes;
  this->con = con;
  this->closes_ix = 0;
  this->days_ix = 0;
  this->sum = 0;
  this->ref = 0;
  return this;
}

// Returns Arr[MABase]
Arr *mABase_cos (int qnicks, QmatrixValues *closes, int days) {
  Arr *r = arr_new();
  RANGE0(i, qnicks)
    arr_push(r, mABase_new(closes, i, days));
  _RANGE
  return r;

}

Order *mABase_order (
  MABase *this, double q, int days, double strip_to_buy, double strip_to_sell
) {
  if (co->days_ix < days) {
    if (q > 0) {
      co->sum += q;
      co->days_ix += 1;
      co->ref = co->sum / co->days_ix;
    }
    return order_none();
  }

  if (q > 0) {
    while (co->closes[co->closes_ix][co->con] <= 0) {
      ++co->closes_ix;
    }

    co->sum += q - co->closes[co->closes_ix][co->con];
    ++co->closes_ix;
    double avg = co->sum / days;
    if (co->to_sell) {
      double ref = co->ref;
      if (avg > ref) {
        co->ref = avg;
      } else {
        avg = ref;
      }
      if (q <= avg * (1 - strip_to_sell)) {
        co->ref = co->sum / days;
        co->to_sell = 0;
        return order_sell();
      } else {
        return order_none();
      }
    } else {
      double ref0 = co->ref;
      if (avg < ref0) {
        co->ref = avg;
      } else {
        avg = ref0;
      }
      double ref = avg * (1 + strip_to_buy);
      if (q >= ref) {
        double pond = (q - ref) / ref;
        co->ref = co->sum / days;
        co->to_sell = 1;
        return order_buy(pond);
      } else {
        return order_none();
      }
    }
  } else {
    return order_none();
  }
}

double mABase_ref (
  MABase *this, double strip_to_buy, double strip_to_sell
) {
  return this->to_sell
    ? this->ref  * (1 - strip_to_sell)
    : this->ref * (1 + strip_to_buy)
  ;
}

