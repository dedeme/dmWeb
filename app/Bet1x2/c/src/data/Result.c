// Copyright 24-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Result.h"
#include "dmc/DEFS.h"
#include "dmc/js.h"

Result *result_new(int home, int out) {
  Result *this = MALLOC(Result);
  this->home = home;
  this->out = out;
  return this;
}

int result_eq(Result *this, Result *other) {
  return this->home == other->home && this->out == other->out;
}

enum cts_BET_TYPE result_value (Result *this) {
  return this->home > this->out ? cts_BET_1
    : this->out > this->home ? cts_BET_2
    : cts_BET_x
  ;
}

char *result_to_js (Result *this) {
  return js_wa(achar_new_from(
    js_wi(this->home),
    js_wi(this->out),
    NULL
  ));
}

Result *result_from_js(char *js) {
  Achar *a = js_ra(js);
  return result_new(
    js_ri(achar_get(a, 0)),
    js_ri(achar_get(a, 1))
  );
}
