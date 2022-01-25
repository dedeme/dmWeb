// Copyright 28-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Decision/AADecision.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AADecision *aADecision_new (void) {
  return (AADecision *)arr_new();
}

AADecision *aADecision_bf_new (int buffer) {
  return (AADecision *)arr_bf_new(buffer);
}

AADecision *aADecision_new_from (ADecision *e, ...) {
  va_list args;
  void *tmp;

  AADecision *this = aADecision_new();
  aADecision_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, ADecision *);
  while (tmp) {
    aADecision_push(this, tmp);
    tmp = va_arg(args, ADecision *);
  }
  va_end(args);

  return this;
}

AADecision *aADecision_new_c (int size, ADecision **es) {
  return (AADecision *)arr_new_c(size, (void **)es);
}

AADecision *aADecision_copy (AADecision *this) {
  return (AADecision *)arr_copy((Arr *)this);
}

int aADecision_size (AADecision *this) {
  return arr_size((Arr *)this);
}

ADecision *aADecision_get (AADecision *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aADecision_push (AADecision *this, ADecision *e) {
  arr_push((Arr *)this, e);
}

ADecision *aADecision_pop (AADecision *this) {
  return arr_pop((Arr *)this);
}

ADecision *aADecision_peek (AADecision *this) {
  return arr_peek((Arr *)this);
}

void aADecision_set (AADecision *this, int ix, ADecision *e) {
  arr_set((Arr *)this, ix, e);
}

void aADecision_insert (AADecision *this, int ix, ADecision *e) {
  arr_insert((Arr *)this, ix, e);
}

void aADecision_remove (AADecision *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aADecision_cat (AADecision *this, AADecision *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aADecision_insert_arr (AADecision *this, int ix, AADecision *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aADecision_remove_range (AADecision *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aADecision_clear (AADecision *this) {
  arr_clear((Arr *)this);
}

void aADecision_reverse (AADecision *this) {
  arr_reverse((Arr *)this);
}

void aADecision_sort (AADecision *this, int (*greater)(ADecision *e1, ADecision *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aADecision_shuffle (AADecision *this) {
  arr_shuffle((Arr *)this);
}

int aADecision_all (AADecision *this, int (*pred)(ADecision *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aADecision_any (AADecision *this, int (*pred)(ADecision *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aADecision_index (AADecision *this, int (*pred)(ADecision *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aADecision_last_index (AADecision *this, int (*pred)(ADecision *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OADecision *aADecision_find(AADecision *this, int (*pred)(ADecision *e)) {
  return (OADecision *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OADecision *aADecision_find_last(AADecision *this, int (*pred)(ADecision *e)) {
  return (OADecision *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aADecision_filter_in (AADecision *this, int (*pred)(ADecision *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AADecision *aADecision_take (AADecision *this, int n) {
  return (AADecision *)arr_take((Arr *)this, n);
}

AADecision *aADecision_takef (AADecision *this, int (*pred)(ADecision *e)) {
  return (AADecision *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AADecision *aADecision_drop (AADecision *this, int n) {
  return (AADecision *)arr_drop((Arr *)this, n);
}

AADecision *aADecision_dropf (AADecision *this, int (*pred)(ADecision *e)) {
  return (AADecision *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AADecision *aADecision_filter_to (AADecision *this, int (*pred)(ADecision *e)) {
  return (AADecision *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aADecision_map (AADecision *this, void *(*converter)(ADecision *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aADecision_map2 (
  AADecision *this, void *(*conv1)(ADecision *e), void *(*conv2)(ADecision *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aADecision_zip (
  AADecision *a1, AADecision *a2, void *(*converter)(ADecision *e1, ADecision *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aADecision_zip3 (
  AADecision *a1, AADecision *a2, AADecision *a3,
  void*(*converter)(ADecision*e1, ADecision*e2, ADecision*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AADecision *aADecision_duplicates (
  AADecision *this, int (feq)(ADecision *e1, ADecision *e2)
) {
  return (AADecision *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aADecision_to_js (AADecision *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))aDecision_to_js);
}

AADecision *aADecision_from_js (char *js) {
  return (AADecision *)arr_from_js(js, (void *(*)(char *))aDecision_from_js);
}


//--// Not remove

