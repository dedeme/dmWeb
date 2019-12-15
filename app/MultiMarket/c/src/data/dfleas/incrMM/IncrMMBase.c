// Copyright 15-Dec-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/dfleas/incrMM/IncrMMBase.h"
IncrMMBase *incrMMBase_new(
  int to_sell, QmatrixValues *closes, int con
) {
  IncrMMBase *this = MALLOC(IncrMMBase);
  this->to_sell = to_sell;
  this->closes = closes;
  this->ref = -1;
  this->con = con;
  this->closes_ix = 0;
  this->days_ix = 0;
  this->day_mm_ix = 0;
  return this;
}

/// Returns Arr[IncrMMBase]
Arr *incrMMBase_cos (int qnicks, QmatrixValues *closes, double strip_to_sell) {
  Arr *r = arr_new();
  RANGE0(i, qnicks)
    arr_push(r, incrMMBase_new(
      1, closes, i)
    );
  _RANGE
  return r;
}

Order *incrMMBase_order (
  IncrMMBase *this, double q, int days,
  double strip_to_buy, double strip_to_sell
) {
  if (this->days_ix < days) {
    double q = this->closes[this->days_ix][this->con];
    if (q > 0 && (this->ref < 0 || q < this->ref)) {
      this->ref = q;
      this->day_mm_ix = this->days_ix;
    }
    ++this->days_ix;
    return order_none();
  }

  ++this->closes_ix;

  int ix = this->day_mm_ix - 1;
  if (ix < 0) {
    QmatrixValues *closes = this->closes;
    int closes_ix = this->closes_ix;
    int con = this->con;
    double ref = this->ref;
    if (this->to_sell) {
      double min = 1000000;
      RANGE0(i, days - 1) {
        double q = closes[closes_ix + i][con];
        if (q > ref && q < min) {
          min = q;
          ix = i;
        }
      }_RANGE
    } else {
      double max = 0;
      RANGE0(i, days - 1) {
        double q = closes[closes_ix + i][con];
        if (q < ref && q > max) {
          max = q;
          ix = i;
        }
      }_RANGE
    }

    if (ix > -1) {
      this->ref = closes[closes_ix + ix][con];
    } else {
      ix = 0;
    }
  }
  this->day_mm_ix = ix;

  double ref = this->ref;
  if (q < 0 || ref < 0) {
    return order_none();
  }


  if (this->to_sell) {
    if (q <= (ref * (1 - strip_to_sell))) {
      QmatrixValues *closes = this->closes;
      int closes_ix = this->closes_ix;
      int con = this->con;
      int ix = days - 1;
      double new_ref = q;
      RANGE0(i, days - 1) {
        double q2 = closes[closes_ix + i][con];
        if (q2 > new_ref) {
          new_ref = q2;
          ix = i;
        }
      }_RANGE
      this->ref = new_ref;
      this->day_mm_ix = ix;

      this->to_sell = 0;
      return order_sell();
    } else {
      return order_none();
    }
  } else {
    ref = ref * (1 + strip_to_buy);
    if (q >= ref) {
      double pond = (q - ref) / ref;

      QmatrixValues *closes = this->closes;
      int closes_ix = this->closes_ix;
      int con = this->con;
      int ix = days - 1;
      double new_ref = q;
      RANGE0(i, days - 1) {
        double q2 = closes[closes_ix + i][con];
        if (q2 > 1 && q2 < new_ref) {
          new_ref = q2;
          ix = i;
        }
      }_RANGE
      this->ref = new_ref;
      this->day_mm_ix = ix;

      this->to_sell = 1;
      return order_buy(pond);
    } else {
      return order_none();
    }
  }
}

double incrMMBase_ref (
  IncrMMBase *this, double strip_to_buy, double strip_to_sell
) {
  return this->to_sell
    ? this->ref * (1 - strip_to_sell)
    : this->ref * (1 + strip_to_buy)
  ;
}

