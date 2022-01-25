// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Result/AAOOResult.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AAOOResult *aAOOResult_new (void) {
  return (AAOOResult *)arr_new();
}

AAOOResult *aAOOResult_bf_new (int buffer) {
  return (AAOOResult *)arr_bf_new(buffer);
}

AAOOResult *aAOOResult_new_from (AOOResult *e, ...) {
  va_list args;
  void *tmp;

  AAOOResult *this = aAOOResult_new();
  aAOOResult_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, AOOResult *);
  while (tmp) {
    aAOOResult_push(this, tmp);
    tmp = va_arg(args, AOOResult *);
  }
  va_end(args);

  return this;
}

AAOOResult *aAOOResult_new_c (int size, AOOResult **es) {
  return (AAOOResult *)arr_new_c(size, (void **)es);
}

AAOOResult *aAOOResult_copy (AAOOResult *this) {
  return (AAOOResult *)arr_copy((Arr *)this);
}

int aAOOResult_size (AAOOResult *this) {
  return arr_size((Arr *)this);
}

AOOResult *aAOOResult_get (AAOOResult *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aAOOResult_push (AAOOResult *this, AOOResult *e) {
  arr_push((Arr *)this, e);
}

AOOResult *aAOOResult_pop (AAOOResult *this) {
  return arr_pop((Arr *)this);
}

AOOResult *aAOOResult_peek (AAOOResult *this) {
  return arr_peek((Arr *)this);
}

void aAOOResult_set (AAOOResult *this, int ix, AOOResult *e) {
  arr_set((Arr *)this, ix, e);
}

void aAOOResult_insert (AAOOResult *this, int ix, AOOResult *e) {
  arr_insert((Arr *)this, ix, e);
}

void aAOOResult_remove (AAOOResult *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aAOOResult_cat (AAOOResult *this, AAOOResult *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aAOOResult_insert_arr (AAOOResult *this, int ix, AAOOResult *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aAOOResult_remove_range (AAOOResult *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aAOOResult_clear (AAOOResult *this) {
  arr_clear((Arr *)this);
}

void aAOOResult_reverse (AAOOResult *this) {
  arr_reverse((Arr *)this);
}

void aAOOResult_sort (AAOOResult *this, int (*greater)(AOOResult *e1, AOOResult *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aAOOResult_shuffle (AAOOResult *this) {
  arr_shuffle((Arr *)this);
}

int aAOOResult_all (AAOOResult *this, int (*pred)(AOOResult *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aAOOResult_any (AAOOResult *this, int (*pred)(AOOResult *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aAOOResult_index (AAOOResult *this, int (*pred)(AOOResult *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aAOOResult_last_index (AAOOResult *this, int (*pred)(AOOResult *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OAOOResult *aAOOResult_find(AAOOResult *this, int (*pred)(AOOResult *e)) {
  return (OAOOResult *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OAOOResult *aAOOResult_find_last(AAOOResult *this, int (*pred)(AOOResult *e)) {
  return (OAOOResult *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aAOOResult_filter_in (AAOOResult *this, int (*pred)(AOOResult *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AAOOResult *aAOOResult_take (AAOOResult *this, int n) {
  return (AAOOResult *)arr_take((Arr *)this, n);
}

AAOOResult *aAOOResult_takef (AAOOResult *this, int (*pred)(AOOResult *e)) {
  return (AAOOResult *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AAOOResult *aAOOResult_drop (AAOOResult *this, int n) {
  return (AAOOResult *)arr_drop((Arr *)this, n);
}

AAOOResult *aAOOResult_dropf (AAOOResult *this, int (*pred)(AOOResult *e)) {
  return (AAOOResult *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AAOOResult *aAOOResult_filter_to (AAOOResult *this, int (*pred)(AOOResult *e)) {
  return (AAOOResult *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aAOOResult_map (AAOOResult *this, void *(*converter)(AOOResult *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aAOOResult_map2 (
  AAOOResult *this, void *(*conv1)(AOOResult *e), void *(*conv2)(AOOResult *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aAOOResult_zip (
  AAOOResult *a1, AAOOResult *a2, void *(*converter)(AOOResult *e1, AOOResult *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aAOOResult_zip3 (
  AAOOResult *a1, AAOOResult *a2, AAOOResult *a3,
  void*(*converter)(AOOResult*e1, AOOResult*e2, AOOResult*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AAOOResult *aAOOResult_duplicates (
  AAOOResult *this, int (feq)(AOOResult *e1, AOOResult *e2)
) {
  return (AAOOResult *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aAOOResult_to_js (AAOOResult *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))aOOResult_to_js);
}

AAOOResult *aAOOResult_from_js (char *js) {
  return (AAOOResult *)arr_from_js(js, (void *(*)(char *))aOOResult_from_js);
}


//--// Not remove

