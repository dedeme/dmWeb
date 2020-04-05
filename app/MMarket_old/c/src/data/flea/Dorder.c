// Copyright 24-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/flea/Dorder.h"

/* .
-Dorder
  is_sell: bool
  co_ix  : int
  pond   : double
*/

/*--*/

struct Dorder_Dorder {
  int is_sell;
  int co_ix;
  double pond;
};

static Dorder *_dorder_new (int is_sell, int co_ix, double pond) {
  Dorder *this = MALLOC(Dorder);
  this->is_sell = is_sell;
  this->co_ix = co_ix;
  this->pond = pond;
  return this;
}

int dorder_is_sell (Dorder *this) {
  return this->is_sell;
}

int dorder_co_ix (Dorder *this) {
  return this->co_ix;
}

double dorder_pond (Dorder *this) {
  return this->pond;
}

/*--*/

Dorder *dorder_sell (int co_ix) {
  return _dorder_new(1, co_ix, 0);
}

Dorder *dorder_buy (int co_ix, double pond) {
  return _dorder_new(0, co_ix, pond);
}

void dorder_sort (Arr *dorders) {
  int fn (Dorder *d1, Dorder *d2) {
    if (d1->is_sell) return 0;
    if (d2->is_sell) return 1;
    return d1->pond > d2->pond;
  }
  arr_sort(dorders, (FCMP)fn);
}
