// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Result/AAAResult.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AAAResult *aAAResult_new (void) {
  return (AAAResult *)arr_new();
}

AAAResult *aAAResult_bf_new (int buffer) {
  return (AAAResult *)arr_bf_new(buffer);
}

AAAResult *aAAResult_new_from (AAResult *e, ...) {
  va_list args;
  void *tmp;

  AAAResult *this = aAAResult_new();
  aAAResult_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, AAResult *);
  while (tmp) {
    aAAResult_push(this, tmp);
    tmp = va_arg(args, AAResult *);
  }
  va_end(args);

  return this;
}

AAAResult *aAAResult_new_c (int size, AAResult **es) {
  return (AAAResult *)arr_new_c(size, (void **)es);
}

AAAResult *aAAResult_copy (AAAResult *this) {
  return (AAAResult *)arr_copy((Arr *)this);
}

int aAAResult_size (AAAResult *this) {
  return arr_size((Arr *)this);
}

AAResult *aAAResult_get (AAAResult *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aAAResult_push (AAAResult *this, AAResult *e) {
  arr_push((Arr *)this, e);
}

AAResult *aAAResult_pop (AAAResult *this) {
  return arr_pop((Arr *)this);
}

AAResult *aAAResult_peek (AAAResult *this) {
  return arr_peek((Arr *)this);
}

void aAAResult_set (AAAResult *this, int ix, AAResult *e) {
  arr_set((Arr *)this, ix, e);
}

void aAAResult_insert (AAAResult *this, int ix, AAResult *e) {
  arr_insert((Arr *)this, ix, e);
}

void aAAResult_remove (AAAResult *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aAAResult_cat (AAAResult *this, AAAResult *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aAAResult_insert_arr (AAAResult *this, int ix, AAAResult *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aAAResult_remove_range (AAAResult *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aAAResult_clear (AAAResult *this) {
  arr_clear((Arr *)this);
}

void aAAResult_reverse (AAAResult *this) {
  arr_reverse((Arr *)this);
}

void aAAResult_sort (AAAResult *this, int (*greater)(AAResult *e1, AAResult *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aAAResult_shuffle (AAAResult *this) {
  arr_shuffle((Arr *)this);
}

int aAAResult_all (AAAResult *this, int (*pred)(AAResult *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aAAResult_any (AAAResult *this, int (*pred)(AAResult *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aAAResult_index (AAAResult *this, int (*pred)(AAResult *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aAAResult_last_index (AAAResult *this, int (*pred)(AAResult *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OAAResult *aAAResult_find(AAAResult *this, int (*pred)(AAResult *e)) {
  return (OAAResult *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OAAResult *aAAResult_find_last(AAAResult *this, int (*pred)(AAResult *e)) {
  return (OAAResult *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aAAResult_filter_in (AAAResult *this, int (*pred)(AAResult *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AAAResult *aAAResult_take (AAAResult *this, int n) {
  return (AAAResult *)arr_take((Arr *)this, n);
}

AAAResult *aAAResult_takef (AAAResult *this, int (*pred)(AAResult *e)) {
  return (AAAResult *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AAAResult *aAAResult_drop (AAAResult *this, int n) {
  return (AAAResult *)arr_drop((Arr *)this, n);
}

AAAResult *aAAResult_dropf (AAAResult *this, int (*pred)(AAResult *e)) {
  return (AAAResult *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AAAResult *aAAResult_filter_to (AAAResult *this, int (*pred)(AAResult *e)) {
  return (AAAResult *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aAAResult_map (AAAResult *this, void *(*converter)(AAResult *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aAAResult_map2 (
  AAAResult *this, void *(*conv1)(AAResult *e), void *(*conv2)(AAResult *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aAAResult_zip (
  AAAResult *a1, AAAResult *a2, void *(*converter)(AAResult *e1, AAResult *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aAAResult_zip3 (
  AAAResult *a1, AAAResult *a2, AAAResult *a3,
  void*(*converter)(AAResult*e1, AAResult*e2, AAResult*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AAAResult *aAAResult_duplicates (
  AAAResult *this, int (feq)(AAResult *e1, AAResult *e2)
) {
  return (AAAResult *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aAAResult_to_js (AAAResult *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))aAResult_to_js);
}

AAAResult *aAAResult_from_js (char *js) {
  return (AAAResult *)arr_from_js(js, (void *(*)(char *))aAResult_from_js);
}


//--// Not remove

