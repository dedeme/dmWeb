// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Timetable.h"

/* .
# Market day time table.
-Timetable: serial
  hopen: int
  mopen: int
  hclose: int
  mclose: int
*/

/*--*/

struct Timetable_Timetable {
  int hopen;
  int mopen;
  int hclose;
  int mclose;
};

static Timetable *_timetable_new (
  int hopen,
  int mopen,
  int hclose,
  int mclose
) {
  Timetable *this = MALLOC(Timetable);
  this->hopen = hopen;
  this->mopen = mopen;
  this->hclose = hclose;
  this->mclose = mclose;
  return this;
}

int timetable_hopen (Timetable *this) {
  return this->hopen;
}

int timetable_mopen (Timetable *this) {
  return this->mopen;
}

int timetable_hclose (Timetable *this) {
  return this->hclose;
}

int timetable_mclose (Timetable *this) {
  return this->mclose;
}

Js *timetable_to_js (Timetable *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_wi((int)this->hopen));
  arr_push(js, js_wi((int)this->mopen));
  arr_push(js, js_wi((int)this->hclose));
  arr_push(js, js_wi((int)this->mclose));
  return js_wa(js);
}

Timetable *timetable_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  Timetable *this = MALLOC(Timetable);
  this->hopen = js_ri(*p++);
  this->mopen = js_ri(*p++);
  this->hclose = js_ri(*p++);
  this->mclose = js_ri(*p++);
  return this;
}

/*--*/

Timetable *timetable_new(void) {
  return _timetable_new(0, 0, 23, 55);
}
