// Copyright 15-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "dmc/Arr.h"
#include "string.h"

#include "dmc/std.h"

static void free_es(void **es, int len, void(*ffree)(void *)) {
  void **p = es;
  REPEAT(len)
    ffree(*p++);
  _REPEAT
  free(es);
}

struct arr_Arr {
  void **es;
  void **end;
  void **endbf;
  void (*ffree)(void *);
};

Arr *arr_new(void(*ffree)(void *)) {
  return arr_bf_new(15, ffree);
}

///
Arr *arr_bf_new(int buffer, void(*ffree)(void *)) {
  Arr *this = malloc(sizeof(Arr));
  void **es = malloc(buffer * sizeof(void *));
  this->es = es;
  this->end = es;
  this->endbf = es + buffer;
  this->ffree = ffree;
  return this;
}


Arr *arr_left_new(Arr *this, int ix, void *(*copy_new)(void *)) {
  Arr *tmp = malloc(sizeof(Arr));
  tmp->es = this->es;
  tmp->end = this->es + ix;
  Arr *r = arr_new(this->ffree);
  arr_cat(r, tmp, copy_new);
  free(tmp);
  return r;
}


Arr *arr_right_new(Arr *this, int ix, void *(*copy_new)(void *)) {
  Arr *tmp = malloc(sizeof(Arr));
  tmp->es = this->es + ix;
  tmp->end = this->end;
  Arr *r = arr_new(this->ffree);
  arr_cat(r, tmp, copy_new);
  free(tmp);
  return r;
}

Arr *arr_sub_new(Arr *this, int begin, int end, void *(*copy_new)(void *)) {
  Arr *r = arr_new(this->ffree);
  if (end > begin) {
    Arr *tmp = malloc(sizeof(Arr));
    tmp->es = this->es + begin;
    tmp->end = this->es + end;
    arr_cat(r, tmp, copy_new);
    free(tmp);
  }
  return r;
}

void arr_free(Arr *this) {
  free_es(this->es, this->end - this->es, this->ffree);
  free(this);
}

int arr_size(Arr *this) {
  return this->end - this->es;
}

void *arr_get(Arr *this, int ix) {
  return *(this->es + ix);
}

void **arr_start(Arr *this) {
  return this->es;
}

void **arr_end(Arr *this) {
  return this->end;
}

FPROC arr_ffree(Arr *this) {
  return this->ffree;
}

void arr_push(Arr *this, void *e) {
  if (this->end == this->endbf) {
    int size = this->endbf - this->es;
    int new_size = size + size;
    this->es = realloc(this->es, new_size * sizeof(void *));
    this->end = this->es + size;
    this->endbf = this->es + new_size;
  }
  *this->end++ = e;
}

void *arr_pop_new(Arr *this) {
  --this->end;
  return *this->end;
}

void *arr_peek(Arr *this) {
  return *(this->end - 1);
}

void arr_set(Arr *this, int ix, void *e) {
  void **p = this->es + ix;
  this->ffree(*p);
  *p = e;
}

void arr_insert(Arr *this, int ix, void *e) {
  Arr *new = arr_bf_new((this->endbf - this->es) + 1, this->ffree);
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
void arr_remove(Arr *this, int ix) {
  Arr *new = arr_bf_new((this->endbf - this->es) - 1, this->ffree);
  void **p = this->es;
  void **p_end = this->end;
  void **t = new->es;
  int c = 0;
  while (p < p_end) {
    if (c == ix) {
      this->ffree(*p);
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

void arr_cat(Arr *this, Arr *other, void *(*copy_new)(void *)) {
  int other_len = other->end - other->es;
  if (other_len) {
    int this_len = this->end - this->es;
    int this_size = this->endbf - this->es;
    if (this_len + other_len >= this_size){
      int new_size = this_size + other_len;
      this->es = realloc(this->es, new_size * sizeof(void *));
      this->end = this->es + this_len;
      this->endbf = this->es + new_size;
    }
    void **s = other->es;
    void **t = this->end;
    REPEAT(other_len)
      *t++ = copy_new(*s++);
    _REPEAT
    this->end = t;
  }
}

void arr_insert_arr(Arr *this, int ix, Arr *other, void *(*copy_new)(void *)) {
  Arr *other_new = arr_new(this->ffree);
  arr_cat(other_new, other, copy_new);
  Varr *left = varr_left_new((Varr *)this, ix);
  Varr *right = varr_right_new((Varr *)this, ix);
  varr_cat(left, (Varr *)other_new);
  varr_cat(left, right);

  free(this->es);
  this->es = ((Arr *)left)->es;
  this->end = ((Arr *)left)->end;
  this->endbf = ((Arr *)left)->endbf;

  varr_free(right);
  varr_free((Varr *)other_new);
  free(left);
}

void arr_remove_range(Arr *this, int begin, int end) {
  void **p = this->es + begin;
  void (*ffree)(void *) = this->ffree;
  REPEAT(end - begin)
    ffree(*p++);
  _REPEAT

  Varr *left = varr_left_new((Varr *)this, begin);
  Varr *right = varr_right_new((Varr *)this, end);
  varr_cat(left, right);

  free(this->es);
  this->es = ((Arr *)left)->es;
  this->end = ((Arr *)left)->end;
  this->endbf = ((Arr *)left)->endbf;

  varr_free(right);
  free(left);
}

void arr_reverse(Arr *this) {
  varr_reverse((Varr *)this);
}

void arr_sort(Arr *this, int (*greater)(void *, void *)) {
  varr_sort((Varr *)this, greater);
}

void arr_shuffle(Arr *this) {
  varr_shuffle((Varr *)this);
}

int arr_index(Arr *this, int (*pred)(void *e)) {
  return varr_index((Varr *)this, pred);
}

void arr_filter(Arr *this, int (*pred)(void *e)) {
  void **p = this->es;
  void **end = this->end;
  void **new_end = p;
  while (p < end) {
    if (pred(*p)) {
      *new_end++ = *p;
    } else {
      this->ffree(*p);
    }
    ++p;
  }
  this->end = new_end;
}

Js *arr_to_js_new(Arr *this, Js *(*to_new)(void *e)) {
  // Arr[Js]
  Arr *a = arr_new(free);
  void **p = this->es;
  void **end = this->end;
  while (p < end) {
    arr_push(a, to_new(*p++));
  }
  Js *r = js_wa_new(a);
  arr_free(a);
  return r;
}

Arr *arr_from_js_new(
  Js *js,
  void *(*from_new)(Js *jse),
  void (*ffree)(void *e)
) {
  Arr *this = arr_new(ffree);
  // Arr[Js]
  Arr *a = js_ra_new(js);
  void **p = a->es;
  void **end = a->end;
  while (p < end) {
    arr_push(this, from_new(*p++));
  }
  arr_free(a);
  return this;
}
