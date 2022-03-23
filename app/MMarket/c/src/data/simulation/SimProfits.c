// Copyright 28-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/simulation/SimProfits.h"
#include "dmc/js.h"
#include "dmc/DEFS.h"

SimProfits *simProfits_new (double total, double cash, double ref) {
  SimProfits *this = MALLOC(SimProfits);
  this->total = total;
  this->cash = cash;
  this->ref = ref;
  return this;
}

SimProfits *simProfits_sum (SimProfits *s1, SimProfits *s2) {
  return simProfits_new(
    s1->total + s2->total,
    s1->cash + s2->cash,
    s1->ref + s2->ref
  );
}

SimProfits *simProfits_div (SimProfits *this, double n) {
  return simProfits_new(
    this->total / n,
    this->cash / n,
    this->ref / n
  );
}

char *simProfits_to_js(SimProfits *this) {
  return js_wa(achar_new_from(
    js_wd(this->total),
    js_wd(this->cash),
    js_wd(this->ref),
    NULL
  ));
}

SimProfits *simProfits_from_js(char *js) {
  Achar *a = js_ra(js);
  return simProfits_new(
    js_rd(achar_get(a, 0)),
    js_rd(achar_get(a, 1)),
    js_rd(achar_get(a, 2))
  );
}
