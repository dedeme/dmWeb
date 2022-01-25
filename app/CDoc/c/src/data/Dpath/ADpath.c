// Copyright 25-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Dpath/ADpath.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

ADpath *aDpath_new (void) {
  return (ADpath *)arr_new();
}

ADpath *aDpath_bf_new (int buffer) {
  return (ADpath *)arr_bf_new(buffer);
}

ADpath *aDpath_new_from (Dpath *e, ...) {
  va_list args;
  void *tmp;

  ADpath *this = aDpath_new();
  aDpath_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, Dpath *);
  while (tmp) {
    aDpath_push(this, tmp);
    tmp = va_arg(args, Dpath *);
  }
  va_end(args);

  return this;
}

ADpath *aDpath_new_c (int size, Dpath **es) {
  return (ADpath *)arr_new_c(size, (void **)es);
}

ADpath *aDpath_copy (ADpath *this) {
  return (ADpath *)arr_copy((Arr *)this);
}

int aDpath_size (ADpath *this) {
  return arr_size((Arr *)this);
}

Dpath *aDpath_get (ADpath *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aDpath_push (ADpath *this, Dpath *e) {
  arr_push((Arr *)this, e);
}

Dpath *aDpath_pop (ADpath *this) {
  return arr_pop((Arr *)this);
}

Dpath *aDpath_peek (ADpath *this) {
  return arr_peek((Arr *)this);
}

void aDpath_set (ADpath *this, int ix, Dpath *e) {
  arr_set((Arr *)this, ix, e);
}

void aDpath_insert (ADpath *this, int ix, Dpath *e) {
  arr_insert((Arr *)this, ix, e);
}

void aDpath_remove (ADpath *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aDpath_cat (ADpath *this, ADpath *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aDpath_insert_arr (ADpath *this, int ix, ADpath *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aDpath_remove_range (ADpath *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aDpath_clear (ADpath *this) {
  arr_clear((Arr *)this);
}

void aDpath_reverse (ADpath *this) {
  arr_reverse((Arr *)this);
}

void aDpath_sort (ADpath *this, int (*greater)(Dpath *e1, Dpath *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aDpath_shuffle (ADpath *this) {
  arr_shuffle((Arr *)this);
}

int aDpath_all (ADpath *this, int (*pred)(Dpath *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aDpath_any (ADpath *this, int (*pred)(Dpath *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aDpath_index (ADpath *this, int (*pred)(Dpath *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aDpath_last_index (ADpath *this, int (*pred)(Dpath *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

ODpath *aDpath_find(ADpath *this, int (*pred)(Dpath *e)) {
  return (ODpath *)arr_find((Arr *)this, (int(*)(void *))pred);
}

ODpath *aDpath_find_last(ADpath *this, int (*pred)(Dpath *e)) {
  return (ODpath *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aDpath_filter_in (ADpath *this, int (*pred)(Dpath *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

ADpath *aDpath_take (ADpath *this, int n) {
  return (ADpath *)arr_take((Arr *)this, n);
}

ADpath *aDpath_takef (ADpath *this, int (*pred)(Dpath *e)) {
  return (ADpath *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

ADpath *aDpath_drop (ADpath *this, int n) {
  return (ADpath *)arr_drop((Arr *)this, n);
}

ADpath *aDpath_dropf (ADpath *this, int (*pred)(Dpath *e)) {
  return (ADpath *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

ADpath *aDpath_filter_to (ADpath *this, int (*pred)(Dpath *e)) {
  return (ADpath *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aDpath_map (ADpath *this, void *(*converter)(Dpath *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aDpath_map2 (
  ADpath *this, void *(*conv1)(Dpath *e), void *(*conv2)(Dpath *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aDpath_zip (
  ADpath *a1, ADpath *a2, void *(*converter)(Dpath *e1, Dpath *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aDpath_zip3 (
  ADpath *a1, ADpath *a2, ADpath *a3,
  void*(*converter)(Dpath*e1, Dpath*e2, Dpath*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

ADpath *aDpath_duplicates (
  ADpath *this, int (feq)(Dpath *e1, Dpath *e2)
) {
  return (ADpath *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aDpath_to_js (ADpath *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))dpath_to_js);
}

ADpath *aDpath_from_js (char *js) {
  return (ADpath *)arr_from_js(js, (void *(*)(char *))dpath_from_js);
}


//--// Not remove

