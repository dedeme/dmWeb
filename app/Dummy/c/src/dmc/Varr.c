// Copyright 15-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "dmc/Varr.h"
#include "string.h"

#include "dmc/std.h"
#include "dmc/rnd.h"

struct varr_Varr {
  void **es;
  void **end;
  void **endbf;
};

Varr *varr_new(void) {
  return varr_2_new(15);
}

///
Varr *varr_2_new(int buffer) {
  Varr *this = malloc(sizeof(Varr));
  void **es = malloc(buffer * sizeof(double));
  this->es = es;
  this->end = es;
  this->endbf = es + buffer;
  return this;
}


Varr *varr_left_new(Varr *this, int ix) {
  if (ix < 0) {
    ix = (this->end - this->es) + ix;
  }
  Varr *tmp = malloc(sizeof(Varr));
  tmp->es = this->es;
  tmp->end = this->es + ix;
  Varr *r = varr_new();
  varr_cat(r, tmp);
  free(tmp);
  return r;
}


Varr *varr_right_new(Varr *this, int ix) {
  if (ix < 0) {
    ix = (this->end - this->es) + ix;
  }
  Varr *tmp = malloc(sizeof(Varr));
  tmp->es = this->es + ix;
  tmp->end = this->end;
  Varr *r = varr_new();
  varr_cat(r, tmp);
  free(tmp);
  return r;
}

Varr *varr_sub_new(Varr *this, int begin, int end) {
  if (begin < 0) {
    begin = (this->end - this->es) + begin;
  }
  if (end < 0) {
    end = (this->end - this->es) + end;
  }
  Varr *r = varr_new();
  if (end > begin) {
    Varr *tmp = malloc(sizeof(Varr));
    tmp->es = this->es + begin;
    tmp->end = this->es + end;
    varr_cat(r, tmp);
    free(tmp);
  }
  return r;
}

void varr_free(Varr *this) {
  free(this->es);
  free(this);
}

int varr_size(Varr *this) {
  return this->end - this->es;
}

void *varr_get(Varr *this, int ix) {
  if (ix < 0) {
    ix = (this->end - this->es) + ix;
  }
  return *(this->es + ix);
}

void **varr_start(Varr *this) {
  return this->es;
}

void **varr_end(Varr *this) {
  return this->end;
}

void varr_push(Varr *this, void *e) {
  if (this->end == this->endbf) {
    int size = this->endbf - this->es;
    int new_size = size + size;
    this->es = realloc(this->es, new_size * sizeof(double));
    this->end = this->es + size;
    this->endbf = this->es + new_size;
  }
  *this->end++ = e;
}

void *varr_pop(Varr *this) {
  --this->end;
  return *this->end;
}

void *varr_peek(Varr *this) {
  return *(this->end - 1);
}

void varr_set(Varr *this, int ix, void *e) {
  if (ix < 0) {
    ix = (this->end - this->es) + ix;
  }
  *(this->es + ix) = e;
}

void varr_insert(Varr *this, int ix, void *e) {
  if (ix < 0) {
    ix = (this->end - this->es) + ix;
  }
  Varr *new = varr_2_new((this->endbf - this->es) + 1);
  void **p = this->es;
  void **p_end = this->end;
  void **t = new->es;
  int c = 0;
  while (p < p_end) {
    if (c == ix) {
      *t++ = e;
      break;
    }
    *t++ = *p++;
    ++c;
  }
  if (p < p_end) {
    while (p < p_end) {
      *t++ = *p++;
    }
  } else {
    *t++ = e;
  }

  free(this->es);
  this->es = new->es;
  this->end = t;
  this->endbf = new->endbf;
  free(new);
}

///
void varr_remove(Varr *this, int ix) {
  if (ix < 0) {
    ix = (this->end - this->es) + ix;
  }
  Varr *new = varr_2_new((this->endbf - this->es) - 1);
  void **p = this->es;
  void **p_end = this->end;
  void **t = new->es;
  int c = 0;
  while (p < p_end) {
    if (c == ix) {
      ++p;
      break;
    }
    *t++ = *p++;
    ++c;
  }
  while (p < p_end) {
    *t++ = *p++;
  }

  free(this->es);
  this->es = new->es;
  this->end = t;
  this->endbf = new->endbf;
  free(new);
}

void varr_cat(Varr *this, Varr *other) {
  int other_len = other->end - other->es;
  if (other_len) {
    int this_len = this->end - this->es;
    int this_size = this->endbf - this->es;
    if (this_len + other_len >= this_size){
      int new_size = this_size + other_len;
      this->es = realloc(this->es, new_size * sizeof(double));
      this->end = this->es + this_len;
      this->endbf = this->es + new_size;
    }
    memcpy(this->end, other->es, other_len * sizeof(double));
    this->end += other_len;
  }
}

void varr_insert_arr(Varr *this, int ix, Varr *other) {
  if (ix < 0) {
    ix = (this->end - this->es) + ix;
  }
  Varr *left = varr_left_new(this, ix);
  Varr *right = varr_right_new(this, ix);
  varr_cat(left, other);
  varr_cat(left, right);
  varr_free(right);
  free(this->es);
  this->es = left->es;
  this->end = left->end;
  this->endbf = left->endbf;
  free(left);
}

void varr_remove_range(Varr *this, int begin, int end) {
  if (begin < 0) {
    begin = (this->end - this->es) + begin;
  }
  if (end < 0) {
    end = (this->end - this->es) + end;
  }
  Varr *left = varr_left_new(this, begin);
  Varr *right = varr_right_new(this, end);
  varr_cat(left, right);
  varr_free(right);
  free(this->es);
  this->es = left->es;
  this->end = left->end;
  this->endbf = left->endbf;
  free(left);
}

void varr_reverse(Varr *this) {
  void **p = this->es;
  void **end = this->end - 1;
  void *tmp;
  REPEAT ((this->end - this->es) / 2) {
    tmp = *p;
    *p++ = *end;
    *end-- = tmp;
  }_REPEAT
}

void varr_sort(Varr *this, int (*greater)(void *, void *)) {
  void sort(void **a, int size) {
    if (size < 2) {
      return;
    }
    int mid1 = size / 2;
    int mid2 = size - mid1;
    void **a1 = malloc(mid1 * sizeof(void *));
    void **a2 = malloc(mid2 * sizeof(void *));
    void **pa = a;
    void **pa1 = a1;
    void **pa2 = a2;
    REPEAT(mid1) {
      *pa1++ = *pa++;
    }_REPEAT
    REPEAT(mid2) {
      *pa2++ = *pa++;
    }_REPEAT
    sort(a1, mid1);
    sort(a2, mid2);

    pa = a;
    int ia1 = 0;
    pa1 = a1;
    int ia2 = 0;
    pa2 = a2;

    for(;;) {
      if (ia1 == mid1) {
        for(;;) {
          if (ia2++ == mid2) {
            break;
          }
          *pa++ = *pa2++;
        }
        break;
      }
      if (ia2 == mid2) {
        for (;;) {
          if (ia1++ == mid1) {
            break;
          }
          *pa++ = *pa1++;
        }
        break;
      }
      if (greater(*pa1, *pa2)) {
        *pa++ = *pa2++;
        ++ia2;
      } else {
        *pa++ = *pa1++;
        ++ia1;
      }
    }
    free(a1);
    free(a2);
  }
  sort(this->es, this->end - this->es);
}

void varr_shuffle(Varr *this) {
  void **es = this->es;
  int size = this->end - es;
  void **p = this->end - 1;
  void **pix, *tmp;
  while (p > es) {
    pix = es + rnd_i(size--);
    tmp = *p;
    *p-- = *(pix);
    *(pix) = tmp;
  }
}

int varr_index(Varr *this, int (*pred)(void *e)) {
  int ix = -1;
  EACH_IX(this, void, e, i)
    if (pred(e)) {
      ix = i;
      break;
    }
  _EACH
  return ix;
}

void varr_filter(Varr *this, int (*pred)(void *e)) {
  void **p = this->es;
  void **end = this->end;
  void **new_end = p;
  while (p < end) {
    if (pred(*p)) {
      *new_end++ = *p;
    }
    ++p;
  }
  this->end = new_end;
}
