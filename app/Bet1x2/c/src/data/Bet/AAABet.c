// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Bet/AAABet.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AAABet *aAABet_new (void) {
  return (AAABet *)arr_new();
}

AAABet *aAABet_bf_new (int buffer) {
  return (AAABet *)arr_bf_new(buffer);
}

AAABet *aAABet_new_from (AABet *e, ...) {
  va_list args;
  void *tmp;

  AAABet *this = aAABet_new();
  aAABet_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, AABet *);
  while (tmp) {
    aAABet_push(this, tmp);
    tmp = va_arg(args, AABet *);
  }
  va_end(args);

  return this;
}

AAABet *aAABet_new_c (int size, AABet **es) {
  return (AAABet *)arr_new_c(size, (void **)es);
}

AAABet *aAABet_copy (AAABet *this) {
  return (AAABet *)arr_copy((Arr *)this);
}

int aAABet_size (AAABet *this) {
  return arr_size((Arr *)this);
}

AABet *aAABet_get (AAABet *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aAABet_push (AAABet *this, AABet *e) {
  arr_push((Arr *)this, e);
}

AABet *aAABet_pop (AAABet *this) {
  return arr_pop((Arr *)this);
}

AABet *aAABet_peek (AAABet *this) {
  return arr_peek((Arr *)this);
}

void aAABet_set (AAABet *this, int ix, AABet *e) {
  arr_set((Arr *)this, ix, e);
}

void aAABet_insert (AAABet *this, int ix, AABet *e) {
  arr_insert((Arr *)this, ix, e);
}

void aAABet_remove (AAABet *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aAABet_cat (AAABet *this, AAABet *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aAABet_insert_arr (AAABet *this, int ix, AAABet *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aAABet_remove_range (AAABet *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aAABet_clear (AAABet *this) {
  arr_clear((Arr *)this);
}

void aAABet_reverse (AAABet *this) {
  arr_reverse((Arr *)this);
}

void aAABet_sort (AAABet *this, int (*greater)(AABet *e1, AABet *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aAABet_shuffle (AAABet *this) {
  arr_shuffle((Arr *)this);
}

int aAABet_all (AAABet *this, int (*pred)(AABet *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aAABet_any (AAABet *this, int (*pred)(AABet *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aAABet_index (AAABet *this, int (*pred)(AABet *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aAABet_last_index (AAABet *this, int (*pred)(AABet *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OAABet *aAABet_find(AAABet *this, int (*pred)(AABet *e)) {
  return (OAABet *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OAABet *aAABet_find_last(AAABet *this, int (*pred)(AABet *e)) {
  return (OAABet *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aAABet_filter_in (AAABet *this, int (*pred)(AABet *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AAABet *aAABet_take (AAABet *this, int n) {
  return (AAABet *)arr_take((Arr *)this, n);
}

AAABet *aAABet_takef (AAABet *this, int (*pred)(AABet *e)) {
  return (AAABet *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AAABet *aAABet_drop (AAABet *this, int n) {
  return (AAABet *)arr_drop((Arr *)this, n);
}

AAABet *aAABet_dropf (AAABet *this, int (*pred)(AABet *e)) {
  return (AAABet *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AAABet *aAABet_filter_to (AAABet *this, int (*pred)(AABet *e)) {
  return (AAABet *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aAABet_map (AAABet *this, void *(*converter)(AABet *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aAABet_map2 (
  AAABet *this, void *(*conv1)(AABet *e), void *(*conv2)(AABet *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aAABet_zip (
  AAABet *a1, AAABet *a2, void *(*converter)(AABet *e1, AABet *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aAABet_zip3 (
  AAABet *a1, AAABet *a2, AAABet *a3,
  void*(*converter)(AABet*e1, AABet*e2, AABet*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AAABet *aAABet_duplicates (
  AAABet *this, int (feq)(AABet *e1, AABet *e2)
) {
  return (AAABet *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aAABet_to_js (AAABet *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))aABet_to_js);
}

AAABet *aAABet_from_js (char *js) {
  return (AAABet *)arr_from_js(js, (void *(*)(char *))aABet_from_js);
}


//--// Not remove

