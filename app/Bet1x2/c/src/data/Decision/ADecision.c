// Copyright 28-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Decision/ADecision.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

ADecision *aDecision_new (void) {
  return (ADecision *)arr_new();
}

ADecision *aDecision_bf_new (int buffer) {
  return (ADecision *)arr_bf_new(buffer);
}

ADecision *aDecision_new_from (Decision *e, ...) {
  va_list args;
  void *tmp;

  ADecision *this = aDecision_new();
  aDecision_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, Decision *);
  while (tmp) {
    aDecision_push(this, tmp);
    tmp = va_arg(args, Decision *);
  }
  va_end(args);

  return this;
}

ADecision *aDecision_new_c (int size, Decision **es) {
  return (ADecision *)arr_new_c(size, (void **)es);
}

ADecision *aDecision_copy (ADecision *this) {
  return (ADecision *)arr_copy((Arr *)this);
}

int aDecision_size (ADecision *this) {
  return arr_size((Arr *)this);
}

Decision *aDecision_get (ADecision *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aDecision_push (ADecision *this, Decision *e) {
  arr_push((Arr *)this, e);
}

Decision *aDecision_pop (ADecision *this) {
  return arr_pop((Arr *)this);
}

Decision *aDecision_peek (ADecision *this) {
  return arr_peek((Arr *)this);
}

void aDecision_set (ADecision *this, int ix, Decision *e) {
  arr_set((Arr *)this, ix, e);
}

void aDecision_insert (ADecision *this, int ix, Decision *e) {
  arr_insert((Arr *)this, ix, e);
}

void aDecision_remove (ADecision *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aDecision_cat (ADecision *this, ADecision *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aDecision_insert_arr (ADecision *this, int ix, ADecision *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aDecision_remove_range (ADecision *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aDecision_clear (ADecision *this) {
  arr_clear((Arr *)this);
}

void aDecision_reverse (ADecision *this) {
  arr_reverse((Arr *)this);
}

void aDecision_sort (ADecision *this, int (*greater)(Decision *e1, Decision *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aDecision_shuffle (ADecision *this) {
  arr_shuffle((Arr *)this);
}

int aDecision_all (ADecision *this, int (*pred)(Decision *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aDecision_any (ADecision *this, int (*pred)(Decision *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aDecision_index (ADecision *this, int (*pred)(Decision *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aDecision_last_index (ADecision *this, int (*pred)(Decision *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

ODecision *aDecision_find(ADecision *this, int (*pred)(Decision *e)) {
  return (ODecision *)arr_find((Arr *)this, (int(*)(void *))pred);
}

ODecision *aDecision_find_last(ADecision *this, int (*pred)(Decision *e)) {
  return (ODecision *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aDecision_filter_in (ADecision *this, int (*pred)(Decision *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

ADecision *aDecision_take (ADecision *this, int n) {
  return (ADecision *)arr_take((Arr *)this, n);
}

ADecision *aDecision_takef (ADecision *this, int (*pred)(Decision *e)) {
  return (ADecision *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

ADecision *aDecision_drop (ADecision *this, int n) {
  return (ADecision *)arr_drop((Arr *)this, n);
}

ADecision *aDecision_dropf (ADecision *this, int (*pred)(Decision *e)) {
  return (ADecision *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

ADecision *aDecision_filter_to (ADecision *this, int (*pred)(Decision *e)) {
  return (ADecision *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aDecision_map (ADecision *this, void *(*converter)(Decision *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aDecision_map2 (
  ADecision *this, void *(*conv1)(Decision *e), void *(*conv2)(Decision *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aDecision_zip (
  ADecision *a1, ADecision *a2, void *(*converter)(Decision *e1, Decision *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aDecision_zip3 (
  ADecision *a1, ADecision *a2, ADecision *a3,
  void*(*converter)(Decision*e1, Decision*e2, Decision*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

ADecision *aDecision_duplicates (
  ADecision *this, int (feq)(Decision *e1, Decision *e2)
) {
  return (ADecision *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aDecision_to_js (ADecision *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))decision_to_js);
}

ADecision *aDecision_from_js (char *js) {
  return (ADecision *)arr_from_js(js, (void *(*)(char *))decision_from_js);
}


//--// Not remove

