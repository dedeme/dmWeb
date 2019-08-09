// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/dfleas/ga/GABase.h"

GABase *gABase_new(QmatrixValues *closes, int con, int days) {
  GABase *this = MALLOC(GABase);
  this->to_sell = 1;
  this->closes = closes;
  this->con = con;
  this->closes_ix = 0;
  this->days_ix = 0;
  this->sum = 0;
  this->num = 0;
  this->dv = days * (1 + days) / 2;
  this->ref = 0;
  return this;
}

// Returns Arr[GABase]
Arr *gABase_cos (int qnicks, QmatrixValues *closes, int days) {
  Arr *r = arr_new();
  RANGE0(i, qnicks)
    arr_push(r, gABase_new(closes, i, days));
  _RANGE
  return r;

}

Order *gABase_order (
  GABase *this, double q, int days, double strip_to_buy, double strip_to_sell
) {
  if (this->days_ix < days) {
    if (q > 0) {
      this->sum += q;
      this->days_ix += 1;
      this->num += q * this->days_ix;
      this->ref = this->num / (this->days_ix * (1 + this->days_ix) / 2);
    }
    return order_none();
  }

  if (q > 0) {
    while (this->closes[this->closes_ix][this->con] <= 0) {
      ++this->closes_ix;
    }
    this->num += q * days - this->sum;
    this->sum += q - this->closes[this->closes_ix][this->con];
    ++this->closes_ix;
    double avg = this->num / this->dv;
    if (this->to_sell) {
      double ref = this->ref;
      if (avg > ref) {
        this->ref = avg;
      } else {
        avg = ref;
      }
      if (q <= avg * (1 - strip_to_sell)) {
        this->ref = this->num / this->dv;
        this->to_sell = 0;
        return order_sell();
      } else {
        return order_none();
      }
    } else {
      double ref0 = this->ref;
      if (avg < ref0) {
        this->ref = avg;
      } else {
        avg = ref0;
      }
      double ref = avg * (1 + strip_to_buy);
      if (q >= ref) {
        double pond = (q - ref) / ref;
        this->ref = this->num / this->dv;
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

double gABase_ref (
  GABase *this, double strip_to_buy, double strip_to_sell
) {
  return this->to_sell
    ? this->ref  * (1 - strip_to_sell)
    : this->ref * (1 + strip_to_buy)
  ;
}
