// Copyright 19-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/flea/Flea.h"
#include "dmc/Dec.h"
#include "dmc/date.h"
#include "DEFS.h"

/* .
# Standar parameters for investments.
Flea: serial
  # Date of creation.
  date: char *
  # Cycle of creation
  cycle: int
  # Identifier number in cycle.
  id: int
  # Parameters of investment.
  gen: Gen
*/
/*--*/

struct Flea_Flea {
  char *date;
  int cycle;
  int id;
  Gen *gen;
};

Flea *flea_new (
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

char *flea_name (Flea *this) {
  return str_f("%s-%d-%d", this->date, this->cycle, this->id);
}

double flea_param(double mx, double mn, double value) {
  return mx > mn ? mn + value * (mx - mn) : 0;
}

int flea_eq (Flea *this, Flea *other) {
  Gen *g1 = this->gen;
  Gen *g2 = other->gen;
  if (gen_size(g1) != gen_size(g2)) return 0;
  double *p1 = gen_params(g1);
  double *p2 = gen_params(g2);
  REPEAT(gen_size(g1)) {
    if (!dec_eq_gap(*p1++, *p2++, 0.000001)) return 0;
  }_REPEAT
  return 1;
}

double flea_evaluate (Flea *this, time_t today, double assets, double profits) {
  double age = date_df(today, date_from_str(this->date));
  double ageRatio = age > HISTORIC_QUOTES? 1.0 : age / HISTORIC_QUOTES;
  return assets * ASSETS_RATIO + profits * PROFITS_RATIO + ageRatio * AGE_RATIO;
}
