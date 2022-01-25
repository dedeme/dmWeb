// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Bet/AAAOBet.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AAAOBet *aAAOBet_new (void) {
  return (AAAOBet *)arr_new();
}

AAAOBet *aAAOBet_bf_new (int buffer) {
  return (AAAOBet *)arr_bf_new(buffer);
}

AAAOBet *aAAOBet_new_from (AAOBet *e, ...) {
  va_list args;
  void *tmp;

  AAAOBet *this = aAAOBet_new();
  aAAOBet_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, AAOBet *);
  while (tmp) {
    aAAOBet_push(this, tmp);
    tmp = va_arg(args, AAOBet *);
  }
  va_end(args);

  return this;
}

AAAOBet *aAAOBet_new_c (int size, AAOBet **es) {
  return (AAAOBet *)arr_new_c(size, (void **)es);
}

AAAOBet *aAAOBet_copy (AAAOBet *this) {
  return (AAAOBet *)arr_copy((Arr *)this);
}

int aAAOBet_size (AAAOBet *this) {
  return arr_size((Arr *)this);
}

AAOBet *aAAOBet_get (AAAOBet *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aAAOBet_push (AAAOBet *this, AAOBet *e) {
  arr_push((Arr *)this, e);
}

AAOBet *aAAOBet_pop (AAAOBet *this) {
  return arr_pop((Arr *)this);
}

AAOBet *aAAOBet_peek (AAAOBet *this) {
  return arr_peek((Arr *)this);
}

void aAAOBet_set (AAAOBet *this, int ix, AAOBet *e) {
  arr_set((Arr *)this, ix, e);
}

void aAAOBet_insert (AAAOBet *this, int ix, AAOBet *e) {
  arr_insert((Arr *)this, ix, e);
}

void aAAOBet_remove (AAAOBet *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aAAOBet_cat (AAAOBet *this, AAAOBet *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aAAOBet_insert_arr (AAAOBet *this, int ix, AAAOBet *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aAAOBet_remove_range (AAAOBet *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aAAOBet_clear (AAAOBet *this) {
  arr_clear((Arr *)this);
}

void aAAOBet_reverse (AAAOBet *this) {
  arr_reverse((Arr *)this);
}

void aAAOBet_sort (AAAOBet *this, int (*greater)(AAOBet *e1, AAOBet *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aAAOBet_shuffle (AAAOBet *this) {
  arr_shuffle((Arr *)this);
}

int aAAOBet_all (AAAOBet *this, int (*pred)(AAOBet *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aAAOBet_any (AAAOBet *this, int (*pred)(AAOBet *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aAAOBet_index (AAAOBet *this, int (*pred)(AAOBet *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aAAOBet_last_index (AAAOBet *this, int (*pred)(AAOBet *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OAAOBet *aAAOBet_find(AAAOBet *this, int (*pred)(AAOBet *e)) {
  return (OAAOBet *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OAAOBet *aAAOBet_find_last(AAAOBet *this, int (*pred)(AAOBet *e)) {
  return (OAAOBet *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aAAOBet_filter_in (AAAOBet *this, int (*pred)(AAOBet *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AAAOBet *aAAOBet_take (AAAOBet *this, int n) {
  return (AAAOBet *)arr_take((Arr *)this, n);
}

AAAOBet *aAAOBet_takef (AAAOBet *this, int (*pred)(AAOBet *e)) {
  return (AAAOBet *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AAAOBet *aAAOBet_drop (AAAOBet *this, int n) {
  return (AAAOBet *)arr_drop((Arr *)this, n);
}

AAAOBet *aAAOBet_dropf (AAAOBet *this, int (*pred)(AAOBet *e)) {
  return (AAAOBet *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AAAOBet *aAAOBet_filter_to (AAAOBet *this, int (*pred)(AAOBet *e)) {
  return (AAAOBet *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aAAOBet_map (AAAOBet *this, void *(*converter)(AAOBet *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aAAOBet_map2 (
  AAAOBet *this, void *(*conv1)(AAOBet *e), void *(*conv2)(AAOBet *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aAAOBet_zip (
  AAAOBet *a1, AAAOBet *a2, void *(*converter)(AAOBet *e1, AAOBet *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aAAOBet_zip3 (
  AAAOBet *a1, AAAOBet *a2, AAAOBet *a3,
  void*(*converter)(AAOBet*e1, AAOBet*e2, AAOBet*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AAAOBet *aAAOBet_duplicates (
  AAAOBet *this, int (feq)(AAOBet *e1, AAOBet *e2)
) {
  return (AAAOBet *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aAAOBet_to_js (AAAOBet *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))aAOBet_to_js);
}

AAAOBet *aAAOBet_from_js (char *js) {
  return (AAAOBet *)arr_from_js(js, (void *(*)(char *))aAOBet_from_js);
}


//--// Not remove

