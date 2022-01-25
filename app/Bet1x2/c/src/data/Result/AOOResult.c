// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Result/AOOResult.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AOOResult *aOOResult_new (void) {
  return (AOOResult *)arr_new();
}

AOOResult *aOOResult_bf_new (int buffer) {
  return (AOOResult *)arr_bf_new(buffer);
}

AOOResult *aOOResult_new_from (OOResult *e, ...) {
  va_list args;
  void *tmp;

  AOOResult *this = aOOResult_new();
  aOOResult_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, OOResult *);
  while (tmp) {
    aOOResult_push(this, tmp);
    tmp = va_arg(args, OOResult *);
  }
  va_end(args);

  return this;
}

AOOResult *aOOResult_new_c (int size, OOResult **es) {
  return (AOOResult *)arr_new_c(size, (void **)es);
}

AOOResult *aOOResult_copy (AOOResult *this) {
  return (AOOResult *)arr_copy((Arr *)this);
}

int aOOResult_size (AOOResult *this) {
  return arr_size((Arr *)this);
}

OOResult *aOOResult_get (AOOResult *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aOOResult_push (AOOResult *this, OOResult *e) {
  arr_push((Arr *)this, e);
}

OOResult *aOOResult_pop (AOOResult *this) {
  return arr_pop((Arr *)this);
}

OOResult *aOOResult_peek (AOOResult *this) {
  return arr_peek((Arr *)this);
}

void aOOResult_set (AOOResult *this, int ix, OOResult *e) {
  arr_set((Arr *)this, ix, e);
}

void aOOResult_insert (AOOResult *this, int ix, OOResult *e) {
  arr_insert((Arr *)this, ix, e);
}

void aOOResult_remove (AOOResult *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aOOResult_cat (AOOResult *this, AOOResult *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aOOResult_insert_arr (AOOResult *this, int ix, AOOResult *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aOOResult_remove_range (AOOResult *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aOOResult_clear (AOOResult *this) {
  arr_clear((Arr *)this);
}

void aOOResult_reverse (AOOResult *this) {
  arr_reverse((Arr *)this);
}

void aOOResult_sort (AOOResult *this, int (*greater)(OOResult *e1, OOResult *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aOOResult_shuffle (AOOResult *this) {
  arr_shuffle((Arr *)this);
}

int aOOResult_all (AOOResult *this, int (*pred)(OOResult *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aOOResult_any (AOOResult *this, int (*pred)(OOResult *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aOOResult_index (AOOResult *this, int (*pred)(OOResult *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aOOResult_last_index (AOOResult *this, int (*pred)(OOResult *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OOOResult *aOOResult_find(AOOResult *this, int (*pred)(OOResult *e)) {
  return (OOOResult *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OOOResult *aOOResult_find_last(AOOResult *this, int (*pred)(OOResult *e)) {
  return (OOOResult *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aOOResult_filter_in (AOOResult *this, int (*pred)(OOResult *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AOOResult *aOOResult_take (AOOResult *this, int n) {
  return (AOOResult *)arr_take((Arr *)this, n);
}

AOOResult *aOOResult_takef (AOOResult *this, int (*pred)(OOResult *e)) {
  return (AOOResult *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AOOResult *aOOResult_drop (AOOResult *this, int n) {
  return (AOOResult *)arr_drop((Arr *)this, n);
}

AOOResult *aOOResult_dropf (AOOResult *this, int (*pred)(OOResult *e)) {
  return (AOOResult *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AOOResult *aOOResult_filter_to (AOOResult *this, int (*pred)(OOResult *e)) {
  return (AOOResult *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aOOResult_map (AOOResult *this, void *(*converter)(OOResult *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aOOResult_map2 (
  AOOResult *this, void *(*conv1)(OOResult *e), void *(*conv2)(OOResult *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aOOResult_zip (
  AOOResult *a1, AOOResult *a2, void *(*converter)(OOResult *e1, OOResult *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aOOResult_zip3 (
  AOOResult *a1, AOOResult *a2, AOOResult *a3,
  void*(*converter)(OOResult*e1, OOResult*e2, OOResult*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AOOResult *aOOResult_duplicates (
  AOOResult *this, int (feq)(OOResult *e1, OOResult *e2)
) {
  return (AOOResult *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aOOResult_to_js (AOOResult *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))oOResult_to_js);
}

AOOResult *aOOResult_from_js (char *js) {
  return (AOOResult *)arr_from_js(js, (void *(*)(char *))oOResult_from_js);
}


//--// Not remove

