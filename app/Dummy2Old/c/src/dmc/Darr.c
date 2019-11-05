// Copyright 15-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "dmc/Darr.h"
#include "string.h"

#include "dmc/std.h"

struct darr_Darr {
  double *es;
  double *end;
  double *endbf;
};

Darr *darr_new(void) {
  return darr_2_new(15);
}

///
Darr *darr_2_new(int buffer) {
  Darr *this = malloc(sizeof(Darr));
  double *es = malloc(buffer * sizeof(double));
  this->es = es;
  this->end = es;
  this->endbf = es + buffer;
  return this;
}


Darr *darr_left_new(Darr *this, int ix) {
  if (ix < 0) {
    ix = (this->end - this->es) + ix;
  }
  Darr *tmp = malloc(sizeof(Darr));
  tmp->es = this->es;
  tmp->end = this->es + ix;
  Darr *r = darr_new();
  darr_cat(r, tmp);
  free(tmp);
  return r;
}


Darr *darr_right_new(Darr *this, int ix) {
  if (ix < 0) {
    ix = (this->end - this->es) + ix;
  }
  Darr *tmp = malloc(sizeof(Darr));
  tmp->es = this->es + ix;
  tmp->end = this->end;
  Darr *r = darr_new();
  darr_cat(r, tmp);
  free(tmp);
  return r;
}

Darr *darr_sub_new(Darr *this, int begin, int end) {
  if (begin < 0) {
    begin = (this->end - this->es) + begin;
  }
  if (end < 0) {
    end = (this->end - this->es) + end;
  }
  Darr *r = darr_new();
  if (end > begin) {
    Darr *tmp = malloc(sizeof(Darr));
    tmp->es = this->es + begin;
    tmp->end = this->es + end;
    darr_cat(r, tmp);
    free(tmp);
  }
  return r;
}

void darr_free(Darr *this) {
  free(this->es);
  free(this);
}

int darr_size(Darr *this) {
  return this->end - this->es;
}

double darr_get(Darr *this, int ix) {
  if (ix < 0) {
    ix = (this->end - this->es) + ix;
  }
  return *(this->es + ix);
}

double *darr_start(Darr *this) {
  return this->es;
}

double *darr_end(Darr *this) {
  return this->end;
}

void darr_push(Darr *this, double e) {
  if (this->end == this->endbf) {
    int size = this->endbf - this->es;
    int new_size = size + size;
    this->es = realloc(this->es, new_size * sizeof(double));
    this->end = this->es + size;
    this->endbf = this->es + new_size;
  }
  *this->end++ = e;
}

void darr_set(Darr *this, int ix, double e) {
  if (ix < 0) {
    ix = (this->end - this->es) + ix;
  }
  *(this->es + ix) = e;
}

void darr_insert(Darr *this, int ix, double e) {
  if (ix < 0) {
    ix = (this->end - this->es) + ix;
  }
  Darr *new = darr_2_new((this->endbf - this->es) + 1);
  double *p = this->es;
  double *p_end = this->end;
  double *t = new->es;
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
void darr_remove(Darr *this, int ix) {
  if (ix < 0) {
    ix = (this->end - this->es) + ix;
  }
  Darr *new = darr_2_new((this->endbf - this->es) - 1);
  double *p = this->es;
  double *p_end = this->end;
  double *t = new->es;
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

void darr_cat(Darr *this, Darr *other) {
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

void darr_insert_arr(Darr *this, int ix, Darr *other) {
  if (ix < 0) {
    ix = (this->end - this->es) + ix;
  }
  Darr *left = darr_left_new(this, ix);
  Darr *right = darr_right_new(this, ix);
  darr_cat(left, other);
  darr_cat(left, right);
  darr_free(right);
  free(this->es);
  this->es = left->es;
  this->end = left->end;
  this->endbf = left->endbf;
  free(left);
}

void darr_remove_range(Darr *this, int begin, int end) {
  if (begin < 0) {
    begin = (this->end - this->es) + begin;
  }
  if (end < 0) {
    end = (this->end - this->es) + end;
  }
  Darr *left = darr_left_new(this, begin);
  Darr *right = darr_right_new(this, end);
  darr_cat(left, right);
  darr_free(right);
  free(this->es);
  this->es = left->es;
  this->end = left->end;
  this->endbf = left->endbf;
  free(left);
}

void darr_reverse(Darr *this) {
  double *p = this->es;
  double *end = this->end - 1;
  double tmp;
  REPEAT ((this->end - this->es) / 2) {
    tmp = *p;
    *p++ = *end;
    *end-- = tmp;
  }_REPEAT
}

void darr_sort(Darr *this) {
  void sort(double *a, int size) {
    if (size < 2) {
      return;
    }
    int mid1 = size / 2;
    int mid2 = size - mid1;
    double *a1 = malloc(mid1 * sizeof(double));
    double *a2 = malloc(mid2 * sizeof(double));
    double *pa = a;
    double *pa1 = a1;
    double *pa2 = a2;
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
      if (*pa1 > *pa2) {
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
