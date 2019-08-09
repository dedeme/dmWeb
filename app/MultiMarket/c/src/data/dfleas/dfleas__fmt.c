// Copyright 01-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/dfleas/dfleas__fmt.h"

static Js *fmt_day = NULL;
static Js *fmt_perc = NULL;

Js *dfleas__fmt_day (void) {
  if (fmt_day) return fmt_day;
  // Arr[Js]
  Arr *r = arr_new();
  arr_push(r, js_ws(""));
  arr_push(r, js_wi(1));
  arr_push(r, js_wi(0));
  arr_push(r, js_ws(""));
  fmt_day = js_wa(r);
  return fmt_day;
}

Js *dfleas__fmt_perc (void) {
  if (fmt_perc) return fmt_perc;
  // Arr[Js]
  Arr *r = arr_new();
  arr_push(r, js_ws(""));
  arr_push(r, js_wi(100));
  arr_push(r, js_wi(4));
  arr_push(r, js_ws("%"));
  fmt_perc = js_wa(r);
  return fmt_perc;
}
