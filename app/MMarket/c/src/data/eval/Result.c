// Copyright 18-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/eval/Result.h"
#include "dmc/DEFS.h"
#include "dmc/js.h"
#include "data/cts.h"

Result *result_new (double assets, double profits, double sales) {
  Result *this = MALLOC(Result);
  this->assets = assets;
  this->profits = profits;
  this->sales = sales;
  return this;
}

double result_eval (Result *this) {
  double max_assets = cts_max_assets_ratio();
  double max_profits = cts_max_profits_avg_ratio();
  double assets = this->assets;
  if (assets > max_assets) assets = max_assets;
  double profits = this->profits;
  if (profits > max_profits) profits = max_profits;

  return
    ( assets * cts_assets_ratio() / max_assets +
      (1 + profits) * cts_profits_avg_ratio() / (1 + max_profits)
    ) / 2
  ;
}

Result *result_sum (Result *r1, Result *r2) {
  return result_new(
    r1->assets + r2->assets,
    r1->profits + r2->profits,
    r1->sales + r2->sales
  );
}

Result *result_div (Result *this, double n) {
  return result_new(
    this->assets / n,
    this->profits / n,
    this->sales /n
  );
}

char *result_to_js (Result *this) {
  return js_wa(achar_new_from(
    js_wd(this->assets),
    js_wd(this->profits),
    js_wd(this->sales),
    NULL
  ));
}
