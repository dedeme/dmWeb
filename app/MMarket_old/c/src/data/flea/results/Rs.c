// Copyright 20-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/flea/results/Rs.h"

/* .
# Results of model_assets
Rs: to
  # Amount
  assets: double
  # Buys number
  buys: int
  # Sells number
  sells: int
*/

/*--*/

struct Rs_Rs {
  double assets;
  int buys;
  int sells;
};

Rs *rs_new (double assets, int buys, int sells) {
  Rs *this = MALLOC(Rs);
  this->assets = assets;
  this->buys = buys;
  this->sells = sells;
  return this;
}

double rs_assets (Rs *this) {
  return this->assets;
}

int rs_buys (Rs *this) {
  return this->buys;
}

int rs_sells (Rs *this) {
  return this->sells;
}

Js *rs_to_js (Rs *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_wd(this->assets));
  arr_push(js, js_wi((int)this->buys));
  arr_push(js, js_wi((int)this->sells));
  return js_wa(js);
}

/*--*/
