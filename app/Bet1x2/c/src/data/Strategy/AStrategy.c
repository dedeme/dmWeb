// Copyright 28-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Strategy/AStrategy.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AStrategy *aStrategy_new (void) {
  return (AStrategy *)arr_new();
}

AStrategy *aStrategy_bf_new (int buffer) {
  return (AStrategy *)arr_bf_new(buffer);
}

AStrategy *aStrategy_new_from (Strategy *e, ...) {
  va_list args;
  void *tmp;

  AStrategy *this = aStrategy_new();
  aStrategy_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, Strategy *);
  while (tmp) {
    aStrategy_push(this, tmp);
    tmp = va_arg(args, Strategy *);
  }
  va_end(args);

  return this;
}

AStrategy *aStrategy_new_c (int size, Strategy **es) {
  return (AStrategy *)arr_new_c(size, (void **)es);
}

AStrategy *aStrategy_copy (AStrategy *this) {
  return (AStrategy *)arr_copy((Arr *)this);
}

int aStrategy_size (AStrategy *this) {
  return arr_size((Arr *)this);
}

Strategy *aStrategy_get (AStrategy *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aStrategy_push (AStrategy *this, Strategy *e) {
  arr_push((Arr *)this, e);
}

Strategy *aStrategy_pop (AStrategy *this) {
  return arr_pop((Arr *)this);
}

Strategy *aStrategy_peek (AStrategy *this) {
  return arr_peek((Arr *)this);
}

void aStrategy_set (AStrategy *this, int ix, Strategy *e) {
  arr_set((Arr *)this, ix, e);
}

void aStrategy_insert (AStrategy *this, int ix, Strategy *e) {
  arr_insert((Arr *)this, ix, e);
}

void aStrategy_remove (AStrategy *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aStrategy_cat (AStrategy *this, AStrategy *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aStrategy_insert_arr (AStrategy *this, int ix, AStrategy *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aStrategy_remove_range (AStrategy *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aStrategy_clear (AStrategy *this) {
  arr_clear((Arr *)this);
}

void aStrategy_reverse (AStrategy *this) {
  arr_reverse((Arr *)this);
}

void aStrategy_sort (AStrategy *this, int (*greater)(Strategy *e1, Strategy *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aStrategy_shuffle (AStrategy *this) {
  arr_shuffle((Arr *)this);
}

int aStrategy_all (AStrategy *this, int (*pred)(Strategy *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aStrategy_any (AStrategy *this, int (*pred)(Strategy *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aStrategy_index (AStrategy *this, int (*pred)(Strategy *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aStrategy_last_index (AStrategy *this, int (*pred)(Strategy *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OStrategy *aStrategy_find(AStrategy *this, int (*pred)(Strategy *e)) {
  return (OStrategy *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OStrategy *aStrategy_find_last(AStrategy *this, int (*pred)(Strategy *e)) {
  return (OStrategy *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aStrategy_filter_in (AStrategy *this, int (*pred)(Strategy *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AStrategy *aStrategy_take (AStrategy *this, int n) {
  return (AStrategy *)arr_take((Arr *)this, n);
}

AStrategy *aStrategy_takef (AStrategy *this, int (*pred)(Strategy *e)) {
  return (AStrategy *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AStrategy *aStrategy_drop (AStrategy *this, int n) {
  return (AStrategy *)arr_drop((Arr *)this, n);
}

AStrategy *aStrategy_dropf (AStrategy *this, int (*pred)(Strategy *e)) {
  return (AStrategy *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AStrategy *aStrategy_filter_to (AStrategy *this, int (*pred)(Strategy *e)) {
  return (AStrategy *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aStrategy_map (AStrategy *this, void *(*converter)(Strategy *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aStrategy_map2 (
  AStrategy *this, void *(*conv1)(Strategy *e), void *(*conv2)(Strategy *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aStrategy_zip (
  AStrategy *a1, AStrategy *a2, void *(*converter)(Strategy *e1, Strategy *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aStrategy_zip3 (
  AStrategy *a1, AStrategy *a2, AStrategy *a3,
  void*(*converter)(Strategy*e1, Strategy*e2, Strategy*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AStrategy *aStrategy_duplicates (
  AStrategy *this, int (feq)(Strategy *e1, Strategy *e2)
) {
  return (AStrategy *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aStrategy_to_js (AStrategy *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))strategy_to_js);
}

AStrategy *aStrategy_from_js (char *js) {
  return (AStrategy *)arr_from_js(js, (void *(*)(char *))strategy_from_js);
}


//--// Not remove

