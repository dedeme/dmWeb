// Copyright 28-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/simulation/SimProfitsRow.h"
#include "dmc/js.h"
#include "dmc/DEFS.h"

SimProfitsRow *simProfitsRow_new (
  ADouble *params, int weeks, SimProfits *hprofits, SimProfits *profits
) {
  SimProfitsRow *this = MALLOC(SimProfitsRow);
  this->params = params;
  this->weeks = weeks;
  this->hprofits = hprofits;
  this->profits = profits;
  return this;
}

char *simProfitsRow_to_js(SimProfitsRow *this) {
  return js_wa(achar_new_from(
    aDouble_to_js(this->params),
    js_wi(this->weeks),
    simProfits_to_js(this->hprofits),
    simProfits_to_js(this->profits),
    NULL
  ));
}

SimProfitsRow *simProfitsRow_from_js(char *js) {
  Achar *a = js_ra(js);
  return simProfitsRow_new(
    aDouble_from_js(achar_get(a, 0)),
    js_ri(achar_get(a, 1)),
    simProfits_from_js(achar_get(a, 2)),
    simProfits_from_js(achar_get(a, 3))
  );
}
