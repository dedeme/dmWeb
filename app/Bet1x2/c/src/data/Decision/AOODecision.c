// Copyright 28-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Decision/AOODecision.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AOODecision *aOODecision_new (void) {
  return (AOODecision *)arr_new();
}

AOODecision *aOODecision_bf_new (int buffer) {
  return (AOODecision *)arr_bf_new(buffer);
}

AOODecision *aOODecision_new_from (OODecision *e, ...) {
  va_list args;
  void *tmp;

  AOODecision *this = aOODecision_new();
  aOODecision_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, OODecision *);
  while (tmp) {
    aOODecision_push(this, tmp);
    tmp = va_arg(args, OODecision *);
  }
  va_end(args);

  return this;
}

AOODecision *aOODecision_new_c (int size, OODecision **es) {
  return (AOODecision *)arr_new_c(size, (void **)es);
}

AOODecision *aOODecision_copy (AOODecision *this) {
  return (AOODecision *)arr_copy((Arr *)this);
}

int aOODecision_size (AOODecision *this) {
  return arr_size((Arr *)this);
}

OODecision *aOODecision_get (AOODecision *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aOODecision_push (AOODecision *this, OODecision *e) {
  arr_push((Arr *)this, e);
}

OODecision *aOODecision_pop (AOODecision *this) {
  return arr_pop((Arr *)this);
}

OODecision *aOODecision_peek (AOODecision *this) {
  return arr_peek((Arr *)this);
}

void aOODecision_set (AOODecision *this, int ix, OODecision *e) {
  arr_set((Arr *)this, ix, e);
}

void aOODecision_insert (AOODecision *this, int ix, OODecision *e) {
  arr_insert((Arr *)this, ix, e);
}

void aOODecision_remove (AOODecision *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aOODecision_cat (AOODecision *this, AOODecision *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aOODecision_insert_arr (AOODecision *this, int ix, AOODecision *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aOODecision_remove_range (AOODecision *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aOODecision_clear (AOODecision *this) {
  arr_clear((Arr *)this);
}

void aOODecision_reverse (AOODecision *this) {
  arr_reverse((Arr *)this);
}

void aOODecision_sort (AOODecision *this, int (*greater)(OODecision *e1, OODecision *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aOODecision_shuffle (AOODecision *this) {
  arr_shuffle((Arr *)this);
}

int aOODecision_all (AOODecision *this, int (*pred)(OODecision *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aOODecision_any (AOODecision *this, int (*pred)(OODecision *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aOODecision_index (AOODecision *this, int (*pred)(OODecision *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aOODecision_last_index (AOODecision *this, int (*pred)(OODecision *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OOODecision *aOODecision_find(AOODecision *this, int (*pred)(OODecision *e)) {
  return (OOODecision *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OOODecision *aOODecision_find_last(AOODecision *this, int (*pred)(OODecision *e)) {
  return (OOODecision *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aOODecision_filter_in (AOODecision *this, int (*pred)(OODecision *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AOODecision *aOODecision_take (AOODecision *this, int n) {
  return (AOODecision *)arr_take((Arr *)this, n);
}

AOODecision *aOODecision_takef (AOODecision *this, int (*pred)(OODecision *e)) {
  return (AOODecision *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AOODecision *aOODecision_drop (AOODecision *this, int n) {
  return (AOODecision *)arr_drop((Arr *)this, n);
}

AOODecision *aOODecision_dropf (AOODecision *this, int (*pred)(OODecision *e)) {
  return (AOODecision *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AOODecision *aOODecision_filter_to (AOODecision *this, int (*pred)(OODecision *e)) {
  return (AOODecision *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aOODecision_map (AOODecision *this, void *(*converter)(OODecision *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aOODecision_map2 (
  AOODecision *this, void *(*conv1)(OODecision *e), void *(*conv2)(OODecision *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aOODecision_zip (
  AOODecision *a1, AOODecision *a2, void *(*converter)(OODecision *e1, OODecision *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aOODecision_zip3 (
  AOODecision *a1, AOODecision *a2, AOODecision *a3,
  void*(*converter)(OODecision*e1, OODecision*e2, OODecision*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AOODecision *aOODecision_duplicates (
  AOODecision *this, int (feq)(OODecision *e1, OODecision *e2)
) {
  return (AOODecision *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aOODecision_to_js (AOODecision *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))oODecision_to_js);
}

AOODecision *aOODecision_from_js (char *js) {
  return (AOODecision *)arr_from_js(js, (void *(*)(char *))oODecision_from_js);
}


//--// Not remove

