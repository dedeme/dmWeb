// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Result/AAOResult.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AAOResult *aAOResult_new (void) {
  return (AAOResult *)arr_new();
}

AAOResult *aAOResult_bf_new (int buffer) {
  return (AAOResult *)arr_bf_new(buffer);
}

AAOResult *aAOResult_new_from (AOResult *e, ...) {
  va_list args;
  void *tmp;

  AAOResult *this = aAOResult_new();
  aAOResult_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, AOResult *);
  while (tmp) {
    aAOResult_push(this, tmp);
    tmp = va_arg(args, AOResult *);
  }
  va_end(args);

  return this;
}

AAOResult *aAOResult_new_c (int size, AOResult **es) {
  return (AAOResult *)arr_new_c(size, (void **)es);
}

AAOResult *aAOResult_copy (AAOResult *this) {
  return (AAOResult *)arr_copy((Arr *)this);
}

int aAOResult_size (AAOResult *this) {
  return arr_size((Arr *)this);
}

AOResult *aAOResult_get (AAOResult *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aAOResult_push (AAOResult *this, AOResult *e) {
  arr_push((Arr *)this, e);
}

AOResult *aAOResult_pop (AAOResult *this) {
  return arr_pop((Arr *)this);
}

AOResult *aAOResult_peek (AAOResult *this) {
  return arr_peek((Arr *)this);
}

void aAOResult_set (AAOResult *this, int ix, AOResult *e) {
  arr_set((Arr *)this, ix, e);
}

void aAOResult_insert (AAOResult *this, int ix, AOResult *e) {
  arr_insert((Arr *)this, ix, e);
}

void aAOResult_remove (AAOResult *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aAOResult_cat (AAOResult *this, AAOResult *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aAOResult_insert_arr (AAOResult *this, int ix, AAOResult *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aAOResult_remove_range (AAOResult *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aAOResult_clear (AAOResult *this) {
  arr_clear((Arr *)this);
}

void aAOResult_reverse (AAOResult *this) {
  arr_reverse((Arr *)this);
}

void aAOResult_sort (AAOResult *this, int (*greater)(AOResult *e1, AOResult *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aAOResult_shuffle (AAOResult *this) {
  arr_shuffle((Arr *)this);
}

int aAOResult_all (AAOResult *this, int (*pred)(AOResult *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aAOResult_any (AAOResult *this, int (*pred)(AOResult *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aAOResult_index (AAOResult *this, int (*pred)(AOResult *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aAOResult_last_index (AAOResult *this, int (*pred)(AOResult *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OAOResult *aAOResult_find(AAOResult *this, int (*pred)(AOResult *e)) {
  return (OAOResult *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OAOResult *aAOResult_find_last(AAOResult *this, int (*pred)(AOResult *e)) {
  return (OAOResult *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aAOResult_filter_in (AAOResult *this, int (*pred)(AOResult *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AAOResult *aAOResult_take (AAOResult *this, int n) {
  return (AAOResult *)arr_take((Arr *)this, n);
}

AAOResult *aAOResult_takef (AAOResult *this, int (*pred)(AOResult *e)) {
  return (AAOResult *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AAOResult *aAOResult_drop (AAOResult *this, int n) {
  return (AAOResult *)arr_drop((Arr *)this, n);
}

AAOResult *aAOResult_dropf (AAOResult *this, int (*pred)(AOResult *e)) {
  return (AAOResult *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AAOResult *aAOResult_filter_to (AAOResult *this, int (*pred)(AOResult *e)) {
  return (AAOResult *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aAOResult_map (AAOResult *this, void *(*converter)(AOResult *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aAOResult_map2 (
  AAOResult *this, void *(*conv1)(AOResult *e), void *(*conv2)(AOResult *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aAOResult_zip (
  AAOResult *a1, AAOResult *a2, void *(*converter)(AOResult *e1, AOResult *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aAOResult_zip3 (
  AAOResult *a1, AAOResult *a2, AAOResult *a3,
  void*(*converter)(AOResult*e1, AOResult*e2, AOResult*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AAOResult *aAOResult_duplicates (
  AAOResult *this, int (feq)(AOResult *e1, AOResult *e2)
) {
  return (AAOResult *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aAOResult_to_js (AAOResult *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))aOResult_to_js);
}

AAOResult *aAOResult_from_js (char *js) {
  return (AAOResult *)arr_from_js(js, (void *(*)(char *))aOResult_from_js);
}


//--// Not remove

#include "data/cts.h"

AAOResult *aAOResult_new_nones (void) {
  AAOResult *this = aAOResult_new();
  for (int i = 0; i < mchar_size(cts_teams()); ++i) {
    aAOResult_push(this, aOResult_new_nones());
  }
  return this;
}
