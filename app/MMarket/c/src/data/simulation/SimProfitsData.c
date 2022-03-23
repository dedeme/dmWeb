// Copyright 28-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/simulation/SimProfitsData.h"
#include "dmc/js.h"
#include "dmc/DEFS.h"

SimProfitsData *simProfitsData_new (char *date, ASimProfitsRow *rows) {
  SimProfitsData *this = MALLOC(SimProfitsData);
  this->date = date;
  this->rows = rows;
  return this;
}

char *simProfitsData_to_js(SimProfitsData *this) {
  return js_wa(achar_new_from(
    js_ws(this->date),
    aSimProfitsRow_to_js(this->rows),
    NULL
  ));
}

SimProfitsData *simProfitsData_from_js(char *js) {
  Achar *a = js_ra(js);
  return simProfitsData_new(
    js_rs(achar_get(a, 0)),
    aSimProfitsRow_from_js(achar_get(a, 1))
  );
}
