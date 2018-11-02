// Copyright 17-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "dmc/rnd.h"
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include "dmc/DEFS.h"
#include "dmc/Arr.h"
#include "dmc/sys.h"

void rnd_init (void) {
  srand(time(0));
}

double rnd_d (void) {
  return (double)rand() / (double)RAND_MAX;
}

int rnd_i (int top) {
  return (int)(rnd_d() * top);
}

struct rnd_Box {
  int i;
  int size;
  Varr *a;
};

Box *rnd_box_new(Varr *a) {
  if (!varr_size(a)) {
    FAIL("rnd_box_new: a is empty")
  }

  Varr *va = varr_new();
  EACH(a, void, e)
    varr_push(va, e);
  _EACH
  varr_shuffle(va);

  Box *this = malloc(sizeof(Box));
  this->i = 0;
  this->size = varr_size(va);
  this->a = va;
  return this;
}

void rnd_box_free(Box *this) {
  varr_free(this->a);
  free(this);
}

/// rnd_box_next returns the next element of 'this'.
void *rnd_box_next(Box *this) {
  if (this->i == this->size) {
    varr_shuffle(this->a);
    this->i = 0;
  }
  return varr_get(this->a, this->i++);
}
