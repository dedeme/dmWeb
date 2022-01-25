// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Bet/AABet.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AABet *aABet_new (void) {
  return (AABet *)arr_new();
}

AABet *aABet_bf_new (int buffer) {
  return (AABet *)arr_bf_new(buffer);
}

AABet *aABet_new_from (ABet *e, ...) {
  va_list args;
  void *tmp;

  AABet *this = aABet_new();
  aABet_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, ABet *);
  while (tmp) {
    aABet_push(this, tmp);
    tmp = va_arg(args, ABet *);
  }
  va_end(args);

  return this;
}

AABet *aABet_new_c (int size, ABet **es) {
  return (AABet *)arr_new_c(size, (void **)es);
}

AABet *aABet_copy (AABet *this) {
  return (AABet *)arr_copy((Arr *)this);
}

int aABet_size (AABet *this) {
  return arr_size((Arr *)this);
}

ABet *aABet_get (AABet *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aABet_push (AABet *this, ABet *e) {
  arr_push((Arr *)this, e);
}

ABet *aABet_pop (AABet *this) {
  return arr_pop((Arr *)this);
}

ABet *aABet_peek (AABet *this) {
  return arr_peek((Arr *)this);
}

void aABet_set (AABet *this, int ix, ABet *e) {
  arr_set((Arr *)this, ix, e);
}

void aABet_insert (AABet *this, int ix, ABet *e) {
  arr_insert((Arr *)this, ix, e);
}

void aABet_remove (AABet *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aABet_cat (AABet *this, AABet *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aABet_insert_arr (AABet *this, int ix, AABet *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aABet_remove_range (AABet *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aABet_clear (AABet *this) {
  arr_clear((Arr *)this);
}

void aABet_reverse (AABet *this) {
  arr_reverse((Arr *)this);
}

void aABet_sort (AABet *this, int (*greater)(ABet *e1, ABet *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aABet_shuffle (AABet *this) {
  arr_shuffle((Arr *)this);
}

int aABet_all (AABet *this, int (*pred)(ABet *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aABet_any (AABet *this, int (*pred)(ABet *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aABet_index (AABet *this, int (*pred)(ABet *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aABet_last_index (AABet *this, int (*pred)(ABet *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OABet *aABet_find(AABet *this, int (*pred)(ABet *e)) {
  return (OABet *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OABet *aABet_find_last(AABet *this, int (*pred)(ABet *e)) {
  return (OABet *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aABet_filter_in (AABet *this, int (*pred)(ABet *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AABet *aABet_take (AABet *this, int n) {
  return (AABet *)arr_take((Arr *)this, n);
}

AABet *aABet_takef (AABet *this, int (*pred)(ABet *e)) {
  return (AABet *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AABet *aABet_drop (AABet *this, int n) {
  return (AABet *)arr_drop((Arr *)this, n);
}

AABet *aABet_dropf (AABet *this, int (*pred)(ABet *e)) {
  return (AABet *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AABet *aABet_filter_to (AABet *this, int (*pred)(ABet *e)) {
  return (AABet *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aABet_map (AABet *this, void *(*converter)(ABet *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aABet_map2 (
  AABet *this, void *(*conv1)(ABet *e), void *(*conv2)(ABet *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aABet_zip (
  AABet *a1, AABet *a2, void *(*converter)(ABet *e1, ABet *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aABet_zip3 (
  AABet *a1, AABet *a2, AABet *a3,
  void*(*converter)(ABet*e1, ABet*e2, ABet*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AABet *aABet_duplicates (
  AABet *this, int (feq)(ABet *e1, ABet *e2)
) {
  return (AABet *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aABet_to_js (AABet *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))aBet_to_js);
}

AABet *aABet_from_js (char *js) {
  return (AABet *)arr_from_js(js, (void *(*)(char *))aBet_from_js);
}


//--// Not remove

