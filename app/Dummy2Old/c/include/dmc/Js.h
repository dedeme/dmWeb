// Copyright 17-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Utilities for managing json strings.

#ifndef DMC_JS_H
  #define DMC_JS_H

#include "dmc/Arr.h"
#include "dmc/Map.h"

/// 'Js' is an alias of 'char'. It is freed with 'free'.
typedef struct js_Js Js;

/// Returns true if json is "null" or false in another case.
int js_is_null (Js *json);

/// Read boolean
int js_rb (Js *json);

/// Read int
int js_ri (Js *json);

/// Read double
double js_rd (Js *json);

/// Read string
char *js_rs_new (Js *json);

/// Read array in an Arr[Js]
Arr *js_ra_new (Js *json);

/// Read object in a Map[js]
Map *js_ro_new (Js *json);
/// Write a null value
Js *js_wn_new(void);

/// Write a boolean value
Js *js_wb_new(int value);

/// Write an int value
Js *js_wi_new(int n);

/// Write a double value. 'scale' is forced between [0 - 9] inclusive
Js *js_wd_new(double n, int scale);

/// Write a string
Js *js_ws_new(const char *s);

/// Write an Arr[Js]
Js *js_wa_new(Arr *a);

/// Write a Map[Js]
Js *js_wo_new(Map *m);

#endif
