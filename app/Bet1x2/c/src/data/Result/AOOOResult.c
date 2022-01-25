// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Result/AOOOResult.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AOOOResult *aOOOResult_new (void) {
  return (AOOOResult *)arr_new();
}

AOOOResult *aOOOResult_bf_new (int buffer) {
  return (AOOOResult *)arr_bf_new(buffer);
}

AOOOResult *aOOOResult_new_from (OOOResult *e, ...) {
  va_list args;
  void *tmp;

  AOOOResult *this = aOOOResult_new();
  aOOOResult_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, OOOResult *);
  while (tmp) {
    aOOOResult_push(this, tmp);
    tmp = va_arg(args, OOOResult *);
  }
  va_end(args);

  return this;
}

AOOOResult *aOOOResult_new_c (int size, OOOResult **es) {
  return (AOOOResult *)arr_new_c(size, (void **)es);
}

AOOOResult *aOOOResult_copy (AOOOResult *this) {
  return (AOOOResult *)arr_copy((Arr *)this);
}

int aOOOResult_size (AOOOResult *this) {
  return arr_size((Arr *)this);
}

OOOResult *aOOOResult_get (AOOOResult *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aOOOResult_push (AOOOResult *this, OOOResult *e) {
  arr_push((Arr *)this, e);
}

OOOResult *aOOOResult_pop (AOOOResult *this) {
  return arr_pop((Arr *)this);
}

OOOResult *aOOOResult_peek (AOOOResult *this) {
  return arr_peek((Arr *)this);
}

void aOOOResult_set (AOOOResult *this, int ix, OOOResult *e) {
  arr_set((Arr *)this, ix, e);
}

void aOOOResult_insert (AOOOResult *this, int ix, OOOResult *e) {
  arr_insert((Arr *)this, ix, e);
}

void aOOOResult_remove (AOOOResult *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aOOOResult_cat (AOOOResult *this, AOOOResult *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aOOOResult_insert_arr (AOOOResult *this, int ix, AOOOResult *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aOOOResult_remove_range (AOOOResult *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aOOOResult_clear (AOOOResult *this) {
  arr_clear((Arr *)this);
}

void aOOOResult_reverse (AOOOResult *this) {
  arr_reverse((Arr *)this);
}

void aOOOResult_sort (AOOOResult *this, int (*greater)(OOOResult *e1, OOOResult *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aOOOResult_shuffle (AOOOResult *this) {
  arr_shuffle((Arr *)this);
}

int aOOOResult_all (AOOOResult *this, int (*pred)(OOOResult *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aOOOResult_any (AOOOResult *this, int (*pred)(OOOResult *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aOOOResult_index (AOOOResult *this, int (*pred)(OOOResult *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aOOOResult_last_index (AOOOResult *this, int (*pred)(OOOResult *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OOOOResult *aOOOResult_find(AOOOResult *this, int (*pred)(OOOResult *e)) {
  return (OOOOResult *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OOOOResult *aOOOResult_find_last(AOOOResult *this, int (*pred)(OOOResult *e)) {
  return (OOOOResult *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aOOOResult_filter_in (AOOOResult *this, int (*pred)(OOOResult *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AOOOResult *aOOOResult_take (AOOOResult *this, int n) {
  return (AOOOResult *)arr_take((Arr *)this, n);
}

AOOOResult *aOOOResult_takef (AOOOResult *this, int (*pred)(OOOResult *e)) {
  return (AOOOResult *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AOOOResult *aOOOResult_drop (AOOOResult *this, int n) {
  return (AOOOResult *)arr_drop((Arr *)this, n);
}

AOOOResult *aOOOResult_dropf (AOOOResult *this, int (*pred)(OOOResult *e)) {
  return (AOOOResult *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AOOOResult *aOOOResult_filter_to (AOOOResult *this, int (*pred)(OOOResult *e)) {
  return (AOOOResult *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aOOOResult_map (AOOOResult *this, void *(*converter)(OOOResult *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aOOOResult_map2 (
  AOOOResult *this, void *(*conv1)(OOOResult *e), void *(*conv2)(OOOResult *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aOOOResult_zip (
  AOOOResult *a1, AOOOResult *a2, void *(*converter)(OOOResult *e1, OOOResult *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aOOOResult_zip3 (
  AOOOResult *a1, AOOOResult *a2, AOOOResult *a3,
  void*(*converter)(OOOResult*e1, OOOResult*e2, OOOResult*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AOOOResult *aOOOResult_duplicates (
  AOOOResult *this, int (feq)(OOOResult *e1, OOOResult *e2)
) {
  return (AOOOResult *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aOOOResult_to_js (AOOOResult *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))oOOResult_to_js);
}

AOOOResult *aOOOResult_from_js (char *js) {
  return (AOOOResult *)arr_from_js(js, (void *(*)(char *))oOOResult_from_js);
}


//--// Not remove

