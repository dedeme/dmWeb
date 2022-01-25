// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Result/AAAOResult.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AAAOResult *aAAOResult_new (void) {
  return (AAAOResult *)arr_new();
}

AAAOResult *aAAOResult_bf_new (int buffer) {
  return (AAAOResult *)arr_bf_new(buffer);
}

AAAOResult *aAAOResult_new_from (AAOResult *e, ...) {
  va_list args;
  void *tmp;

  AAAOResult *this = aAAOResult_new();
  aAAOResult_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, AAOResult *);
  while (tmp) {
    aAAOResult_push(this, tmp);
    tmp = va_arg(args, AAOResult *);
  }
  va_end(args);

  return this;
}

AAAOResult *aAAOResult_new_c (int size, AAOResult **es) {
  return (AAAOResult *)arr_new_c(size, (void **)es);
}

AAAOResult *aAAOResult_copy (AAAOResult *this) {
  return (AAAOResult *)arr_copy((Arr *)this);
}

int aAAOResult_size (AAAOResult *this) {
  return arr_size((Arr *)this);
}

AAOResult *aAAOResult_get (AAAOResult *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aAAOResult_push (AAAOResult *this, AAOResult *e) {
  arr_push((Arr *)this, e);
}

AAOResult *aAAOResult_pop (AAAOResult *this) {
  return arr_pop((Arr *)this);
}

AAOResult *aAAOResult_peek (AAAOResult *this) {
  return arr_peek((Arr *)this);
}

void aAAOResult_set (AAAOResult *this, int ix, AAOResult *e) {
  arr_set((Arr *)this, ix, e);
}

void aAAOResult_insert (AAAOResult *this, int ix, AAOResult *e) {
  arr_insert((Arr *)this, ix, e);
}

void aAAOResult_remove (AAAOResult *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aAAOResult_cat (AAAOResult *this, AAAOResult *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aAAOResult_insert_arr (AAAOResult *this, int ix, AAAOResult *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aAAOResult_remove_range (AAAOResult *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aAAOResult_clear (AAAOResult *this) {
  arr_clear((Arr *)this);
}

void aAAOResult_reverse (AAAOResult *this) {
  arr_reverse((Arr *)this);
}

void aAAOResult_sort (AAAOResult *this, int (*greater)(AAOResult *e1, AAOResult *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aAAOResult_shuffle (AAAOResult *this) {
  arr_shuffle((Arr *)this);
}

int aAAOResult_all (AAAOResult *this, int (*pred)(AAOResult *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aAAOResult_any (AAAOResult *this, int (*pred)(AAOResult *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aAAOResult_index (AAAOResult *this, int (*pred)(AAOResult *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aAAOResult_last_index (AAAOResult *this, int (*pred)(AAOResult *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OAAOResult *aAAOResult_find(AAAOResult *this, int (*pred)(AAOResult *e)) {
  return (OAAOResult *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OAAOResult *aAAOResult_find_last(AAAOResult *this, int (*pred)(AAOResult *e)) {
  return (OAAOResult *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aAAOResult_filter_in (AAAOResult *this, int (*pred)(AAOResult *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AAAOResult *aAAOResult_take (AAAOResult *this, int n) {
  return (AAAOResult *)arr_take((Arr *)this, n);
}

AAAOResult *aAAOResult_takef (AAAOResult *this, int (*pred)(AAOResult *e)) {
  return (AAAOResult *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AAAOResult *aAAOResult_drop (AAAOResult *this, int n) {
  return (AAAOResult *)arr_drop((Arr *)this, n);
}

AAAOResult *aAAOResult_dropf (AAAOResult *this, int (*pred)(AAOResult *e)) {
  return (AAAOResult *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AAAOResult *aAAOResult_filter_to (AAAOResult *this, int (*pred)(AAOResult *e)) {
  return (AAAOResult *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aAAOResult_map (AAAOResult *this, void *(*converter)(AAOResult *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aAAOResult_map2 (
  AAAOResult *this, void *(*conv1)(AAOResult *e), void *(*conv2)(AAOResult *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aAAOResult_zip (
  AAAOResult *a1, AAAOResult *a2, void *(*converter)(AAOResult *e1, AAOResult *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aAAOResult_zip3 (
  AAAOResult *a1, AAAOResult *a2, AAAOResult *a3,
  void*(*converter)(AAOResult*e1, AAOResult*e2, AAOResult*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AAAOResult *aAAOResult_duplicates (
  AAAOResult *this, int (feq)(AAOResult *e1, AAOResult *e2)
) {
  return (AAAOResult *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aAAOResult_to_js (AAAOResult *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))aAOResult_to_js);
}

AAAOResult *aAAOResult_from_js (char *js) {
  return (AAAOResult *)arr_from_js(js, (void *(*)(char *))aAOResult_from_js);
}


//--// Not remove

