// Copyright 11-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Gen.h"
#include "dmc/rnd.h"
#include "DEFS.h"

Gen *gen_new(int n) {
  Darr *this = darr_bf_new(n);
  REPEAT(n)
    darr_push(this, 0.5);
  _REPEAT
  return this;
}

int gen_n(Gen *this) {
  return darr_size(this);
}

double *gen_values(Gen *this) {
  return darr_start(this);
}

Gen *gen_mutate(Gen *this) {
  int size = darr_size(this);
  Darr *rs = darr_bf_new(size);
  double *p = darr_start(this);
  REPEAT(size)
    double mul = MUTATION_MULTIPLIER * (rnd_d() * 2 - 1);
    double n = *p++;
    darr_push(rs, mul <= 0 ? n * (1 + mul) : n + (1 - n) * mul);
  _REPEAT
  return rs;
}

Gen *gen_copy(Gen *this) {
  Darr *r = darr_bf_new(darr_size(this));
  DEACH((Darr *)this, d)
    darr_push(r, d);
  _EACH
  return (Gen *)r;
}

Js *gen_to_js(Gen *this) {
  return darr_to_js(this);
}

Gen *gen_from_js(Js *js) {
  return (Gen *)darr_from_js(js);
}
