// Copyright 24-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Bet.h"
#include "dmc/DEFS.h"
#include "dmc/js.h"

Bet *bet_new(double r1, double rx, double r2) {
  Bet *this = MALLOC(Bet);
  this->r1 = r1;
  this->rx = rx;
  this->r2 = r2;
  return this;
}

double bet_incomes (Bet *this, enum cts_BET_TYPE bet) {
  return bet == cts_BET_1 ? this->r1
    : bet == cts_BET_x ? this->rx
    : this->r2
  ;
}

char *bet_to_js (Bet *this) {
  return js_wa(achar_new_from(
    js_wd(this->r1),
    js_wd(this->rx),
    js_wd(this->r2),
    NULL
  ));
}

Bet *bet_from_js(char *js) {
  Achar *a = js_ra(js);
  return bet_new(
    js_rd(achar_get(a, 0)),
    js_rd(achar_get(a, 1)),
    js_rd(achar_get(a, 2))
  );
}
