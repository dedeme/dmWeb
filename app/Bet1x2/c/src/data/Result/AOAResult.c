// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Result/AOAResult.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AOAResult *aOAResult_new (void) {
  return (AOAResult *)arr_new();
}

AOAResult *aOAResult_bf_new (int buffer) {
  return (AOAResult *)arr_bf_new(buffer);
}

AOAResult *aOAResult_new_from (OAResult *e, ...) {
  va_list args;
  void *tmp;

  AOAResult *this = aOAResult_new();
  aOAResult_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, OAResult *);
  while (tmp) {
    aOAResult_push(this, tmp);
    tmp = va_arg(args, OAResult *);
  }
  va_end(args);

  return this;
}

AOAResult *aOAResult_new_c (int size, OAResult **es) {
  return (AOAResult *)arr_new_c(size, (void **)es);
}

AOAResult *aOAResult_copy (AOAResult *this) {
  return (AOAResult *)arr_copy((Arr *)this);
}

int aOAResult_size (AOAResult *this) {
  return arr_size((Arr *)this);
}

OAResult *aOAResult_get (AOAResult *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aOAResult_push (AOAResult *this, OAResult *e) {
  arr_push((Arr *)this, e);
}

OAResult *aOAResult_pop (AOAResult *this) {
  return arr_pop((Arr *)this);
}

OAResult *aOAResult_peek (AOAResult *this) {
  return arr_peek((Arr *)this);
}

void aOAResult_set (AOAResult *this, int ix, OAResult *e) {
  arr_set((Arr *)this, ix, e);
}

void aOAResult_insert (AOAResult *this, int ix, OAResult *e) {
  arr_insert((Arr *)this, ix, e);
}

void aOAResult_remove (AOAResult *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aOAResult_cat (AOAResult *this, AOAResult *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aOAResult_insert_arr (AOAResult *this, int ix, AOAResult *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aOAResult_remove_range (AOAResult *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aOAResult_clear (AOAResult *this) {
  arr_clear((Arr *)this);
}

void aOAResult_reverse (AOAResult *this) {
  arr_reverse((Arr *)this);
}

void aOAResult_sort (AOAResult *this, int (*greater)(OAResult *e1, OAResult *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aOAResult_shuffle (AOAResult *this) {
  arr_shuffle((Arr *)this);
}

int aOAResult_all (AOAResult *this, int (*pred)(OAResult *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aOAResult_any (AOAResult *this, int (*pred)(OAResult *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aOAResult_index (AOAResult *this, int (*pred)(OAResult *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aOAResult_last_index (AOAResult *this, int (*pred)(OAResult *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OOAResult *aOAResult_find(AOAResult *this, int (*pred)(OAResult *e)) {
  return (OOAResult *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OOAResult *aOAResult_find_last(AOAResult *this, int (*pred)(OAResult *e)) {
  return (OOAResult *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aOAResult_filter_in (AOAResult *this, int (*pred)(OAResult *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AOAResult *aOAResult_take (AOAResult *this, int n) {
  return (AOAResult *)arr_take((Arr *)this, n);
}

AOAResult *aOAResult_takef (AOAResult *this, int (*pred)(OAResult *e)) {
  return (AOAResult *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AOAResult *aOAResult_drop (AOAResult *this, int n) {
  return (AOAResult *)arr_drop((Arr *)this, n);
}

AOAResult *aOAResult_dropf (AOAResult *this, int (*pred)(OAResult *e)) {
  return (AOAResult *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AOAResult *aOAResult_filter_to (AOAResult *this, int (*pred)(OAResult *e)) {
  return (AOAResult *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aOAResult_map (AOAResult *this, void *(*converter)(OAResult *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aOAResult_map2 (
  AOAResult *this, void *(*conv1)(OAResult *e), void *(*conv2)(OAResult *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aOAResult_zip (
  AOAResult *a1, AOAResult *a2, void *(*converter)(OAResult *e1, OAResult *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aOAResult_zip3 (
  AOAResult *a1, AOAResult *a2, AOAResult *a3,
  void*(*converter)(OAResult*e1, OAResult*e2, OAResult*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AOAResult *aOAResult_duplicates (
  AOAResult *this, int (feq)(OAResult *e1, OAResult *e2)
) {
  return (AOAResult *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aOAResult_to_js (AOAResult *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))oAResult_to_js);
}

AOAResult *aOAResult_from_js (char *js) {
  return (AOAResult *)arr_from_js(js, (void *(*)(char *))oAResult_from_js);
}


//--// Not remove

