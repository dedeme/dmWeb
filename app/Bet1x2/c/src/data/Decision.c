// Copyright 28-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Decision.h"
#include "dmc/js.h"
#include "dmc/DEFS.h"

Decision *decision_new(
  Result *result, Result *points, Bet *bet, enum cts_BET_TYPE decision
) {
  Decision *this = MALLOC(Decision);
  this->result = result;
  this->points = points;
  this->bet = bet;
  this->decision = decision;
  return this;
}

char *decision_to_js (Decision *this) {
  return js_wa(achar_new_from(
    result_to_js(this->result),
    result_to_js(this->points),
    bet_to_js(this->bet),
    js_wi(this->decision),
    NULL
  ));
}

Decision *decision_from_js(char *js) {
  Achar *a = js_ra(js);
  return decision_new(
    result_from_js(achar_get(a, 0)),
    result_from_js(achar_get(a, 1)),
    bet_from_js(achar_get(a, 2)),
    js_ri(achar_get(a, 3))
  );
}
