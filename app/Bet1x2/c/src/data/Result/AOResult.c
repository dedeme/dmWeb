// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Result/AOResult.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AOResult *aOResult_new (void) {
  return (AOResult *)arr_new();
}

AOResult *aOResult_bf_new (int buffer) {
  return (AOResult *)arr_bf_new(buffer);
}

AOResult *aOResult_new_from (OResult *e, ...) {
  va_list args;
  void *tmp;

  AOResult *this = aOResult_new();
  aOResult_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, OResult *);
  while (tmp) {
    aOResult_push(this, tmp);
    tmp = va_arg(args, OResult *);
  }
  va_end(args);

  return this;
}

AOResult *aOResult_new_c (int size, OResult **es) {
  return (AOResult *)arr_new_c(size, (void **)es);
}

AOResult *aOResult_copy (AOResult *this) {
  return (AOResult *)arr_copy((Arr *)this);
}

int aOResult_size (AOResult *this) {
  return arr_size((Arr *)this);
}

OResult *aOResult_get (AOResult *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aOResult_push (AOResult *this, OResult *e) {
  arr_push((Arr *)this, e);
}

OResult *aOResult_pop (AOResult *this) {
  return arr_pop((Arr *)this);
}

OResult *aOResult_peek (AOResult *this) {
  return arr_peek((Arr *)this);
}

void aOResult_set (AOResult *this, int ix, OResult *e) {
  arr_set((Arr *)this, ix, e);
}

void aOResult_insert (AOResult *this, int ix, OResult *e) {
  arr_insert((Arr *)this, ix, e);
}

void aOResult_remove (AOResult *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aOResult_cat (AOResult *this, AOResult *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aOResult_insert_arr (AOResult *this, int ix, AOResult *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aOResult_remove_range (AOResult *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aOResult_clear (AOResult *this) {
  arr_clear((Arr *)this);
}

void aOResult_reverse (AOResult *this) {
  arr_reverse((Arr *)this);
}

void aOResult_sort (AOResult *this, int (*greater)(OResult *e1, OResult *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aOResult_shuffle (AOResult *this) {
  arr_shuffle((Arr *)this);
}

int aOResult_all (AOResult *this, int (*pred)(OResult *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aOResult_any (AOResult *this, int (*pred)(OResult *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aOResult_index (AOResult *this, int (*pred)(OResult *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aOResult_last_index (AOResult *this, int (*pred)(OResult *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OOResult *aOResult_find(AOResult *this, int (*pred)(OResult *e)) {
  return (OOResult *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OOResult *aOResult_find_last(AOResult *this, int (*pred)(OResult *e)) {
  return (OOResult *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aOResult_filter_in (AOResult *this, int (*pred)(OResult *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AOResult *aOResult_take (AOResult *this, int n) {
  return (AOResult *)arr_take((Arr *)this, n);
}

AOResult *aOResult_takef (AOResult *this, int (*pred)(OResult *e)) {
  return (AOResult *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AOResult *aOResult_drop (AOResult *this, int n) {
  return (AOResult *)arr_drop((Arr *)this, n);
}

AOResult *aOResult_dropf (AOResult *this, int (*pred)(OResult *e)) {
  return (AOResult *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AOResult *aOResult_filter_to (AOResult *this, int (*pred)(OResult *e)) {
  return (AOResult *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aOResult_map (AOResult *this, void *(*converter)(OResult *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aOResult_map2 (
  AOResult *this, void *(*conv1)(OResult *e), void *(*conv2)(OResult *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aOResult_zip (
  AOResult *a1, AOResult *a2, void *(*converter)(OResult *e1, OResult *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aOResult_zip3 (
  AOResult *a1, AOResult *a2, AOResult *a3,
  void*(*converter)(OResult*e1, OResult*e2, OResult*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AOResult *aOResult_duplicates (
  AOResult *this, int (feq)(OResult *e1, OResult *e2)
) {
  return (AOResult *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aOResult_to_js (AOResult *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))oResult_to_js);
}

AOResult *aOResult_from_js (char *js) {
  return (AOResult *)arr_from_js(js, (void *(*)(char *))oResult_from_js);
}


//--// Not remove

#include "data/cts.h"

AOResult *aOResult_new_nones (void) {
  AOResult *this = aOResult_new();
  for (int i = 0; i < mchar_size(cts_teams()); ++i) {
    aOResult_push(this, oResult_mk_none());
  }
  return this;
}
