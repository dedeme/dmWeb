// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Result/AResult.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AResult *aResult_new (void) {
  return (AResult *)arr_new();
}

AResult *aResult_bf_new (int buffer) {
  return (AResult *)arr_bf_new(buffer);
}

AResult *aResult_new_from (Result *e, ...) {
  va_list args;
  void *tmp;

  AResult *this = aResult_new();
  aResult_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, Result *);
  while (tmp) {
    aResult_push(this, tmp);
    tmp = va_arg(args, Result *);
  }
  va_end(args);

  return this;
}

AResult *aResult_new_c (int size, Result **es) {
  return (AResult *)arr_new_c(size, (void **)es);
}

AResult *aResult_copy (AResult *this) {
  return (AResult *)arr_copy((Arr *)this);
}

int aResult_size (AResult *this) {
  return arr_size((Arr *)this);
}

Result *aResult_get (AResult *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aResult_push (AResult *this, Result *e) {
  arr_push((Arr *)this, e);
}

Result *aResult_pop (AResult *this) {
  return arr_pop((Arr *)this);
}

Result *aResult_peek (AResult *this) {
  return arr_peek((Arr *)this);
}

void aResult_set (AResult *this, int ix, Result *e) {
  arr_set((Arr *)this, ix, e);
}

void aResult_insert (AResult *this, int ix, Result *e) {
  arr_insert((Arr *)this, ix, e);
}

void aResult_remove (AResult *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aResult_cat (AResult *this, AResult *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aResult_insert_arr (AResult *this, int ix, AResult *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aResult_remove_range (AResult *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aResult_clear (AResult *this) {
  arr_clear((Arr *)this);
}

void aResult_reverse (AResult *this) {
  arr_reverse((Arr *)this);
}

void aResult_sort (AResult *this, int (*greater)(Result *e1, Result *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aResult_shuffle (AResult *this) {
  arr_shuffle((Arr *)this);
}

int aResult_all (AResult *this, int (*pred)(Result *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aResult_any (AResult *this, int (*pred)(Result *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aResult_index (AResult *this, int (*pred)(Result *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aResult_last_index (AResult *this, int (*pred)(Result *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OResult *aResult_find(AResult *this, int (*pred)(Result *e)) {
  return (OResult *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OResult *aResult_find_last(AResult *this, int (*pred)(Result *e)) {
  return (OResult *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aResult_filter_in (AResult *this, int (*pred)(Result *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AResult *aResult_take (AResult *this, int n) {
  return (AResult *)arr_take((Arr *)this, n);
}

AResult *aResult_takef (AResult *this, int (*pred)(Result *e)) {
  return (AResult *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AResult *aResult_drop (AResult *this, int n) {
  return (AResult *)arr_drop((Arr *)this, n);
}

AResult *aResult_dropf (AResult *this, int (*pred)(Result *e)) {
  return (AResult *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AResult *aResult_filter_to (AResult *this, int (*pred)(Result *e)) {
  return (AResult *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aResult_map (AResult *this, void *(*converter)(Result *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aResult_map2 (
  AResult *this, void *(*conv1)(Result *e), void *(*conv2)(Result *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aResult_zip (
  AResult *a1, AResult *a2, void *(*converter)(Result *e1, Result *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aResult_zip3 (
  AResult *a1, AResult *a2, AResult *a3,
  void*(*converter)(Result*e1, Result*e2, Result*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AResult *aResult_duplicates (
  AResult *this, int (feq)(Result *e1, Result *e2)
) {
  return (AResult *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aResult_to_js (AResult *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))result_to_js);
}

AResult *aResult_from_js (char *js) {
  return (AResult *)arr_from_js(js, (void *(*)(char *))result_from_js);
}


//--// Not remove

