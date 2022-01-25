// Copyright 16-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/ADouble/AADouble.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AADouble *aADouble_new (void) {
  return (AADouble *)arr_new();
}

AADouble *aADouble_bf_new (int buffer) {
  return (AADouble *)arr_bf_new(buffer);
}

AADouble *aADouble_new_from (ADouble *e, ...) {
  va_list args;
  void *tmp;

  AADouble *this = aADouble_new();
  aADouble_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, ADouble *);
  while (tmp) {
    aADouble_push(this, tmp);
    tmp = va_arg(args, ADouble *);
  }
  va_end(args);

  return this;
}

AADouble *aADouble_new_c (int size, ADouble **es) {
  return (AADouble *)arr_new_c(size, (void **)es);
}

AADouble *aADouble_copy (AADouble *this) {
  return (AADouble *)arr_copy((Arr *)this);
}

int aADouble_size (AADouble *this) {
  return arr_size((Arr *)this);
}

ADouble *aADouble_get (AADouble *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aADouble_push (AADouble *this, ADouble *e) {
  arr_push((Arr *)this, e);
}

ADouble *aADouble_pop (AADouble *this) {
  return arr_pop((Arr *)this);
}

ADouble *aADouble_peek (AADouble *this) {
  return arr_peek((Arr *)this);
}

void aADouble_set (AADouble *this, int ix, ADouble *e) {
  arr_set((Arr *)this, ix, e);
}

void aADouble_insert (AADouble *this, int ix, ADouble *e) {
  arr_insert((Arr *)this, ix, e);
}

void aADouble_remove (AADouble *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aADouble_cat (AADouble *this, AADouble *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aADouble_insert_arr (AADouble *this, int ix, AADouble *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aADouble_remove_range (AADouble *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aADouble_clear (AADouble *this) {
  arr_clear((Arr *)this);
}

void aADouble_reverse (AADouble *this) {
  arr_reverse((Arr *)this);
}

void aADouble_sort (AADouble *this, int (*greater)(ADouble *e1, ADouble *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aADouble_shuffle (AADouble *this) {
  arr_shuffle((Arr *)this);
}

int aADouble_all (AADouble *this, int (*pred)(ADouble *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aADouble_any (AADouble *this, int (*pred)(ADouble *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aADouble_index (AADouble *this, int (*pred)(ADouble *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aADouble_last_index (AADouble *this, int (*pred)(ADouble *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OADouble *aADouble_find(AADouble *this, int (*pred)(ADouble *e)) {
  return (OADouble *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OADouble *aADouble_find_last(AADouble *this, int (*pred)(ADouble *e)) {
  return (OADouble *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aADouble_filter_in (AADouble *this, int (*pred)(ADouble *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AADouble *aADouble_take (AADouble *this, int n) {
  return (AADouble *)arr_take((Arr *)this, n);
}

AADouble *aADouble_takef (AADouble *this, int (*pred)(ADouble *e)) {
  return (AADouble *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AADouble *aADouble_drop (AADouble *this, int n) {
  return (AADouble *)arr_drop((Arr *)this, n);
}

AADouble *aADouble_dropf (AADouble *this, int (*pred)(ADouble *e)) {
  return (AADouble *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AADouble *aADouble_filter_to (AADouble *this, int (*pred)(ADouble *e)) {
  return (AADouble *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aADouble_map (AADouble *this, void *(*converter)(ADouble *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aADouble_map2 (
  AADouble *this, void *(*conv1)(ADouble *e), void *(*conv2)(ADouble *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aADouble_zip (
  AADouble *a1, AADouble *a2, void *(*converter)(ADouble *e1, ADouble *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aADouble_zip3 (
  AADouble *a1, AADouble *a2, AADouble *a3,
  void*(*converter)(ADouble*e1, ADouble*e2, ADouble*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AADouble *aADouble_duplicates (
  AADouble *this, int (feq)(ADouble *e1, ADouble *e2)
) {
  return (AADouble *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aADouble_to_js (AADouble *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))aDouble_to_js);
}

AADouble *aADouble_from_js (char *js) {
  return (AADouble *)arr_from_js(js, (void *(*)(char *))aDouble_from_js);
}


//--// Not remove

