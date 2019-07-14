// Copyright 11-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Flea.h"

/* .
-Flea: serial
  date: char *
  cycle: int
  id: int
  gen: Gen
*/
/*--*/

struct Flea_Flea {
  char *date;
  int cycle;
  int id;
  Gen *gen;
};

static Flea *_flea_new (
  char *date,
  int cycle,
  int id,
  Gen *gen
) {
  Flea *this = MALLOC(Flea);
  this->date = date;
  this->cycle = cycle;
  this->id = id;
  this->gen = gen;
  return this;
}

char *flea_date (Flea *this) {
  return this->date;
}

int flea_cycle (Flea *this) {
  return this->cycle;
}

int flea_id (Flea *this) {
  return this->id;
}

Gen *flea_gen (Flea *this) {
  return this->gen;
}

Js *flea_to_js (Flea *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(this->date));
  arr_push(js, js_wi((int)this->cycle));
  arr_push(js, js_wi((int)this->id));
  arr_push(js, gen_to_js(this->gen));
  return js_wa(js);
}

Flea *flea_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  Flea *this = MALLOC(Flea);
  this->date = js_rs(*p++);
  this->cycle = js_ri(*p++);
  this->id = js_ri(*p++);
  this->gen = gen_from_js(*p++);
  return this;
}

/*--*/

Flea *flea_new(char *date, int cycle, int id, int n) {
  return _flea_new(date, cycle, id, gen_new(n));
}

Flea *flea_mutate(Flea *this, char *date, int cycle, int id) {
  return _flea_new(date, cycle, id, gen_mutate(this->gen));
}

/// Returns date-cycle-id
char *flea_name (Flea *this) {
  return str_f("%s-%d-%d", this->date, this->cycle, this->id);
}

double flea_param(double mx, double mn, double value) {
  return mn + value * (mx - mn);
}

