// Copyright 22-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Exe.h"
#include "dmc/DEFS.h"
#include "dmc/str.h"
#include "dmc/sys.h"
#include "dmc/js.h"
#include "dmc/char/Achar.h"

Exe *exe_new (int id, char *day) {
  Exe *this = MALLOC(Exe);
  this->id =id;
  this->day = day;
  return this;
}

char *exe_to_js (Exe *this) {
  return js_wa(achar_new_from(
    js_wi(this->id),
    js_ws(this->day),
    NULL
  ));
}

Exe *exe_from_js (char *js) {
  char **p = js_ra(js)->es;
  Exe *this = MALLOC(Exe);
  this->id = js_ri(*p++);
  this->day = js_rs(*p++);
  return this;
}
