// Copyright 28-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Decision/AAODecision.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AAODecision *aAODecision_new (void) {
  return (AAODecision *)arr_new();
}

AAODecision *aAODecision_bf_new (int buffer) {
  return (AAODecision *)arr_bf_new(buffer);
}

AAODecision *aAODecision_new_from (AODecision *e, ...) {
  va_list args;
  void *tmp;

  AAODecision *this = aAODecision_new();
  aAODecision_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, AODecision *);
  while (tmp) {
    aAODecision_push(this, tmp);
    tmp = va_arg(args, AODecision *);
  }
  va_end(args);

  return this;
}

AAODecision *aAODecision_new_c (int size, AODecision **es) {
  return (AAODecision *)arr_new_c(size, (void **)es);
}

AAODecision *aAODecision_copy (AAODecision *this) {
  return (AAODecision *)arr_copy((Arr *)this);
}

int aAODecision_size (AAODecision *this) {
  return arr_size((Arr *)this);
}

AODecision *aAODecision_get (AAODecision *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aAODecision_push (AAODecision *this, AODecision *e) {
  arr_push((Arr *)this, e);
}

AODecision *aAODecision_pop (AAODecision *this) {
  return arr_pop((Arr *)this);
}

AODecision *aAODecision_peek (AAODecision *this) {
  return arr_peek((Arr *)this);
}

void aAODecision_set (AAODecision *this, int ix, AODecision *e) {
  arr_set((Arr *)this, ix, e);
}

void aAODecision_insert (AAODecision *this, int ix, AODecision *e) {
  arr_insert((Arr *)this, ix, e);
}

void aAODecision_remove (AAODecision *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aAODecision_cat (AAODecision *this, AAODecision *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aAODecision_insert_arr (AAODecision *this, int ix, AAODecision *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aAODecision_remove_range (AAODecision *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aAODecision_clear (AAODecision *this) {
  arr_clear((Arr *)this);
}

void aAODecision_reverse (AAODecision *this) {
  arr_reverse((Arr *)this);
}

void aAODecision_sort (AAODecision *this, int (*greater)(AODecision *e1, AODecision *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aAODecision_shuffle (AAODecision *this) {
  arr_shuffle((Arr *)this);
}

int aAODecision_all (AAODecision *this, int (*pred)(AODecision *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aAODecision_any (AAODecision *this, int (*pred)(AODecision *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aAODecision_index (AAODecision *this, int (*pred)(AODecision *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aAODecision_last_index (AAODecision *this, int (*pred)(AODecision *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OAODecision *aAODecision_find(AAODecision *this, int (*pred)(AODecision *e)) {
  return (OAODecision *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OAODecision *aAODecision_find_last(AAODecision *this, int (*pred)(AODecision *e)) {
  return (OAODecision *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aAODecision_filter_in (AAODecision *this, int (*pred)(AODecision *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AAODecision *aAODecision_take (AAODecision *this, int n) {
  return (AAODecision *)arr_take((Arr *)this, n);
}

AAODecision *aAODecision_takef (AAODecision *this, int (*pred)(AODecision *e)) {
  return (AAODecision *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AAODecision *aAODecision_drop (AAODecision *this, int n) {
  return (AAODecision *)arr_drop((Arr *)this, n);
}

AAODecision *aAODecision_dropf (AAODecision *this, int (*pred)(AODecision *e)) {
  return (AAODecision *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AAODecision *aAODecision_filter_to (AAODecision *this, int (*pred)(AODecision *e)) {
  return (AAODecision *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aAODecision_map (AAODecision *this, void *(*converter)(AODecision *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aAODecision_map2 (
  AAODecision *this, void *(*conv1)(AODecision *e), void *(*conv2)(AODecision *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aAODecision_zip (
  AAODecision *a1, AAODecision *a2, void *(*converter)(AODecision *e1, AODecision *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aAODecision_zip3 (
  AAODecision *a1, AAODecision *a2, AAODecision *a3,
  void*(*converter)(AODecision*e1, AODecision*e2, AODecision*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AAODecision *aAODecision_duplicates (
  AAODecision *this, int (feq)(AODecision *e1, AODecision *e2)
) {
  return (AAODecision *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aAODecision_to_js (AAODecision *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))aODecision_to_js);
}

AAODecision *aAODecision_from_js (char *js) {
  return (AAODecision *)arr_from_js(js, (void *(*)(char *))aODecision_from_js);
}


//--// Not remove

