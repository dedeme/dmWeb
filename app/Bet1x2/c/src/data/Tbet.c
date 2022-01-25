// Copyright 25-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Tbet.h"
#include "dmc/DEFS.h"
#include "dmc/js.h"

Tbet *tbet_new(int day, char *home, char *out, Bet *bet) {
  Tbet *this = MALLOC(Tbet);
  this->day = day;
  this->home = home;
  this->out = out;
  this->bet = bet;
  return this;
}

char *tbet_to_js (Tbet *this) {
  return js_wa(achar_new_from(
    js_wi(this->day),
    js_ws(this->home),
    js_ws(this->out),
    bet_to_js(this->bet),
    NULL
  ));
}

Tbet *tbet_from_js(char *js) {
  Achar *a = js_ra(js);
  return tbet_new(
    js_ri(achar_get(a, 0)),
    js_rs(achar_get(a, 1)),
    js_rs(achar_get(a, 2)),
    bet_from_js(achar_get(a, 3))
  );
}
