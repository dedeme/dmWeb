// Copyright 15-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "dmc/Iarr.h"
#include "string.h"

#include "dmc/std.h"

struct iarr_Iarr {
  int *es;
  int *end;
  int *endbf;
};

Iarr *iarr_new(void) {
  return iarr_2_new(15);
}

///
Iarr *iarr_2_new(int buffer) {
  Iarr *this = malloc(sizeof(Iarr));
  int *es = malloc(buffer * sizeof(int));
  this->es = es;
  this->end = es;
  this->endbf = es + buffer;
  return this;
}


Iarr *iarr_left_new(Iarr *this, int ix) {
  if (ix < 0) {
    ix = (this->end - this->es) + ix;
  }
  Iarr *tmp = malloc(sizeof(Iarr));
  tmp->es = this->es;
  tmp->end = this->es + ix;
  Iarr *r = iarr_new();
  iarr_cat(r, tmp);
  free(tmp);
  return r;
}


Iarr *iarr_right_new(Iarr *this, int ix) {
  if (ix < 0) {
    ix = (this->end - this->es) + ix;
  }
  Iarr *tmp = malloc(sizeof(Iarr));
  tmp->es = this->es + ix;
  tmp->end = this->end;
  Iarr *r = iarr_new();
  iarr_cat(r, tmp);
  free(tmp);
  return r;
}

Iarr *iarr_sub_new(Iarr *this, int begin, int end) {
  if (begin < 0) {
    begin = (this->end - this->es) + begin;
  }
  if (end < 0) {
    end = (this->end - this->es) + end;
  }
  Iarr *r = iarr_new();
  if (end > begin) {
    Iarr *tmp = malloc(sizeof(Iarr));
    tmp->es = this->es + begin;
    tmp->end = this->es + end;
    iarr_cat(r, tmp);
    free(tmp);
  }
  return r;
}

void iarr_free(Iarr *this) {
  free(this->es);
  free(this);
}

int iarr_size(Iarr *this) {
  return this->end - this->es;
}

int iarr_get(Iarr *this, int ix) {
  if (ix < 0) {
    ix = (this->end - this->es) + ix;
  }
  return *(this->es + ix);
}

int *iarr_start(Iarr *this) {
  return this->es;
}

int *iarr_end(Iarr *this) {
  return this->end;
}

void iarr_push(Iarr *this, int e) {
  if (this->end == this->endbf) {
    int size = this->endbf - this->es;
    int new_size = size + size;
    this->es = realloc(this->es, new_size * sizeof(int));
    this->end = this->es + size;
    this->endbf = this->es + new_size;
  }
  *this->end++ = e;
}

void iarr_set(Iarr *this, int ix, int e) {
  if (ix < 0) {
    ix = (this->end - this->es) + ix;
  }
  *(this->es + ix) = e;
}

void iarr_insert(Iarr *this, int ix, int e) {
  if (ix < 0) {
    ix = (this->end - this->es) + ix;
  }
  Iarr *new = iarr_2_new((this->endbf - this->es) + 1);
  int *p = this->es;
  int *p_end = this->end;
  int *t = new->es;
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
void iarr_remove(Iarr *this, int ix) {
  if (ix < 0) {
    ix = (this->end - this->es) + ix;
  }
  Iarr *new = iarr_2_new((this->endbf - this->es) - 1);
  int *p = this->es;
  int *p_end = this->end;
  int *t = new->es;
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

void iarr_cat(Iarr *this, Iarr *other) {
  int other_len = other->end - other->es;
  if (other_len) {
    int this_len = this->end - this->es;
    int this_size = this->endbf - this->es;
    if (this_len + other_len >= this_size){
      int new_size = this_size + other_len;
      this->es = realloc(this->es, new_size * sizeof(int));
      this->end = this->es + this_len;
      this->endbf = this->es + new_size;
    }
    memcpy(this->end, other->es, other_len * sizeof(int));
    this->end += other_len;
  }
}

void iarr_insert_arr(Iarr *this, int ix, Iarr *other) {
  if (ix < 0) {
    ix = (this->end - this->es) + ix;
  }
  Iarr *left = iarr_left_new(this, ix);
  Iarr *right = iarr_right_new(this, ix);
  iarr_cat(left, other);
  iarr_cat(left, right);
  iarr_free(right);
  free(this->es);
  this->es = left->es;
  this->end = left->end;
  this->endbf = left->endbf;
  free(left);
}

void iarr_remove_range(Iarr *this, int begin, int end) {
  if (begin < 0) {
    begin = (this->end - this->es) + begin;
  }
  if (end < 0) {
    end = (this->end - this->es) + end;
  }
  Iarr *left = iarr_left_new(this, begin);
  Iarr *right = iarr_right_new(this, end);
  iarr_cat(left, right);
  iarr_free(right);
  free(this->es);
  this->es = left->es;
  this->end = left->end;
  this->endbf = left->endbf;
  free(left);
}

void iarr_reverse(Iarr *this) {
  int *p = this->es;
  int *end = this->end - 1;
  int tmp;
  REPEAT ((this->end - this->es) / 2) {
    tmp = *p;
    *p++ = *end;
    *end-- = tmp;
  }_REPEAT
}

void iarr_sort(Iarr *this) {
  void sort(int *a, int size) {
    if (size < 2) {
      return;
    }
    int mid1 = size / 2;
    int mid2 = size - mid1;
    int *a1 = malloc(mid1 * sizeof(int));
    int *a2 = malloc(mid2 * sizeof(int));
    int *pa = a;
    int *pa1 = a1;
    int *pa2 = a2;
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
