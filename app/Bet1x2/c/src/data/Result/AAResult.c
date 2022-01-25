// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Result/AAResult.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AAResult *aAResult_new (void) {
  return (AAResult *)arr_new();
}

AAResult *aAResult_bf_new (int buffer) {
  return (AAResult *)arr_bf_new(buffer);
}

AAResult *aAResult_new_from (AResult *e, ...) {
  va_list args;
  void *tmp;

  AAResult *this = aAResult_new();
  aAResult_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, AResult *);
  while (tmp) {
    aAResult_push(this, tmp);
    tmp = va_arg(args, AResult *);
  }
  va_end(args);

  return this;
}

AAResult *aAResult_new_c (int size, AResult **es) {
  return (AAResult *)arr_new_c(size, (void **)es);
}

AAResult *aAResult_copy (AAResult *this) {
  return (AAResult *)arr_copy((Arr *)this);
}

int aAResult_size (AAResult *this) {
  return arr_size((Arr *)this);
}

AResult *aAResult_get (AAResult *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aAResult_push (AAResult *this, AResult *e) {
  arr_push((Arr *)this, e);
}

AResult *aAResult_pop (AAResult *this) {
  return arr_pop((Arr *)this);
}

AResult *aAResult_peek (AAResult *this) {
  return arr_peek((Arr *)this);
}

void aAResult_set (AAResult *this, int ix, AResult *e) {
  arr_set((Arr *)this, ix, e);
}

void aAResult_insert (AAResult *this, int ix, AResult *e) {
  arr_insert((Arr *)this, ix, e);
}

void aAResult_remove (AAResult *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aAResult_cat (AAResult *this, AAResult *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aAResult_insert_arr (AAResult *this, int ix, AAResult *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aAResult_remove_range (AAResult *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aAResult_clear (AAResult *this) {
  arr_clear((Arr *)this);
}

void aAResult_reverse (AAResult *this) {
  arr_reverse((Arr *)this);
}

void aAResult_sort (AAResult *this, int (*greater)(AResult *e1, AResult *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aAResult_shuffle (AAResult *this) {
  arr_shuffle((Arr *)this);
}

int aAResult_all (AAResult *this, int (*pred)(AResult *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aAResult_any (AAResult *this, int (*pred)(AResult *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aAResult_index (AAResult *this, int (*pred)(AResult *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aAResult_last_index (AAResult *this, int (*pred)(AResult *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OAResult *aAResult_find(AAResult *this, int (*pred)(AResult *e)) {
  return (OAResult *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OAResult *aAResult_find_last(AAResult *this, int (*pred)(AResult *e)) {
  return (OAResult *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aAResult_filter_in (AAResult *this, int (*pred)(AResult *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AAResult *aAResult_take (AAResult *this, int n) {
  return (AAResult *)arr_take((Arr *)this, n);
}

AAResult *aAResult_takef (AAResult *this, int (*pred)(AResult *e)) {
  return (AAResult *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AAResult *aAResult_drop (AAResult *this, int n) {
  return (AAResult *)arr_drop((Arr *)this, n);
}

AAResult *aAResult_dropf (AAResult *this, int (*pred)(AResult *e)) {
  return (AAResult *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AAResult *aAResult_filter_to (AAResult *this, int (*pred)(AResult *e)) {
  return (AAResult *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aAResult_map (AAResult *this, void *(*converter)(AResult *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aAResult_map2 (
  AAResult *this, void *(*conv1)(AResult *e), void *(*conv2)(AResult *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aAResult_zip (
  AAResult *a1, AAResult *a2, void *(*converter)(AResult *e1, AResult *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aAResult_zip3 (
  AAResult *a1, AAResult *a2, AAResult *a3,
  void*(*converter)(AResult*e1, AResult*e2, AResult*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AAResult *aAResult_duplicates (
  AAResult *this, int (feq)(AResult *e1, AResult *e2)
) {
  return (AAResult *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aAResult_to_js (AAResult *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))aResult_to_js);
}

AAResult *aAResult_from_js (char *js) {
  return (AAResult *)arr_from_js(js, (void *(*)(char *))aResult_from_js);
}


//--// Not remove

