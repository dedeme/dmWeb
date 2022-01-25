// Copyright 28-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Decision/AODecision.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AODecision *aODecision_new (void) {
  return (AODecision *)arr_new();
}

AODecision *aODecision_bf_new (int buffer) {
  return (AODecision *)arr_bf_new(buffer);
}

AODecision *aODecision_new_from (ODecision *e, ...) {
  va_list args;
  void *tmp;

  AODecision *this = aODecision_new();
  aODecision_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, ODecision *);
  while (tmp) {
    aODecision_push(this, tmp);
    tmp = va_arg(args, ODecision *);
  }
  va_end(args);

  return this;
}

AODecision *aODecision_new_c (int size, ODecision **es) {
  return (AODecision *)arr_new_c(size, (void **)es);
}

AODecision *aODecision_copy (AODecision *this) {
  return (AODecision *)arr_copy((Arr *)this);
}

int aODecision_size (AODecision *this) {
  return arr_size((Arr *)this);
}

ODecision *aODecision_get (AODecision *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aODecision_push (AODecision *this, ODecision *e) {
  arr_push((Arr *)this, e);
}

ODecision *aODecision_pop (AODecision *this) {
  return arr_pop((Arr *)this);
}

ODecision *aODecision_peek (AODecision *this) {
  return arr_peek((Arr *)this);
}

void aODecision_set (AODecision *this, int ix, ODecision *e) {
  arr_set((Arr *)this, ix, e);
}

void aODecision_insert (AODecision *this, int ix, ODecision *e) {
  arr_insert((Arr *)this, ix, e);
}

void aODecision_remove (AODecision *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aODecision_cat (AODecision *this, AODecision *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aODecision_insert_arr (AODecision *this, int ix, AODecision *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aODecision_remove_range (AODecision *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aODecision_clear (AODecision *this) {
  arr_clear((Arr *)this);
}

void aODecision_reverse (AODecision *this) {
  arr_reverse((Arr *)this);
}

void aODecision_sort (AODecision *this, int (*greater)(ODecision *e1, ODecision *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aODecision_shuffle (AODecision *this) {
  arr_shuffle((Arr *)this);
}

int aODecision_all (AODecision *this, int (*pred)(ODecision *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aODecision_any (AODecision *this, int (*pred)(ODecision *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aODecision_index (AODecision *this, int (*pred)(ODecision *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aODecision_last_index (AODecision *this, int (*pred)(ODecision *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OODecision *aODecision_find(AODecision *this, int (*pred)(ODecision *e)) {
  return (OODecision *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OODecision *aODecision_find_last(AODecision *this, int (*pred)(ODecision *e)) {
  return (OODecision *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aODecision_filter_in (AODecision *this, int (*pred)(ODecision *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AODecision *aODecision_take (AODecision *this, int n) {
  return (AODecision *)arr_take((Arr *)this, n);
}

AODecision *aODecision_takef (AODecision *this, int (*pred)(ODecision *e)) {
  return (AODecision *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AODecision *aODecision_drop (AODecision *this, int n) {
  return (AODecision *)arr_drop((Arr *)this, n);
}

AODecision *aODecision_dropf (AODecision *this, int (*pred)(ODecision *e)) {
  return (AODecision *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AODecision *aODecision_filter_to (AODecision *this, int (*pred)(ODecision *e)) {
  return (AODecision *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aODecision_map (AODecision *this, void *(*converter)(ODecision *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aODecision_map2 (
  AODecision *this, void *(*conv1)(ODecision *e), void *(*conv2)(ODecision *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aODecision_zip (
  AODecision *a1, AODecision *a2, void *(*converter)(ODecision *e1, ODecision *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aODecision_zip3 (
  AODecision *a1, AODecision *a2, AODecision *a3,
  void*(*converter)(ODecision*e1, ODecision*e2, ODecision*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AODecision *aODecision_duplicates (
  AODecision *this, int (feq)(ODecision *e1, ODecision *e2)
) {
  return (AODecision *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aODecision_to_js (AODecision *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))oDecision_to_js);
}

AODecision *aODecision_from_js (char *js) {
  return (AODecision *)arr_from_js(js, (void *(*)(char *))oDecision_from_js);
}


//--// Not remove

