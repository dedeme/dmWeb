// Copyright 29-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/NoLost.h"
#include "dmc/js.h"
#include "dmc/DEFS.h"

NoLost *noLost_new (ADouble *assets, AOrderNL *orders) {
  NoLost *this = MALLOC(NoLost);
  this->assets = assets;
  this->orders = orders;
  return this;
}

char *noLost_to_js (NoLost *this) {
  return js_wa(achar_new_from(
    aDouble_to_js(this->assets),
    aOrderNL_to_js(this->orders),
    NULL
  ));
}

NoLost *noLost_from_js (char *js) {
  Achar *a = js_ra(js);
  return noLost_new(
    aDouble_from_js(achar_get(a, 0)),
    aOrderNL_from_js(achar_get(a, 1))
  );
}
