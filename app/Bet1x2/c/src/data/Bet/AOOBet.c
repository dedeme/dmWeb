// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Bet/AOOBet.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AOOBet *aOOBet_new (void) {
  return (AOOBet *)arr_new();
}

AOOBet *aOOBet_bf_new (int buffer) {
  return (AOOBet *)arr_bf_new(buffer);
}

AOOBet *aOOBet_new_from (OOBet *e, ...) {
  va_list args;
  void *tmp;

  AOOBet *this = aOOBet_new();
  aOOBet_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, OOBet *);
  while (tmp) {
    aOOBet_push(this, tmp);
    tmp = va_arg(args, OOBet *);
  }
  va_end(args);

  return this;
}

AOOBet *aOOBet_new_c (int size, OOBet **es) {
  return (AOOBet *)arr_new_c(size, (void **)es);
}

AOOBet *aOOBet_copy (AOOBet *this) {
  return (AOOBet *)arr_copy((Arr *)this);
}

int aOOBet_size (AOOBet *this) {
  return arr_size((Arr *)this);
}

OOBet *aOOBet_get (AOOBet *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aOOBet_push (AOOBet *this, OOBet *e) {
  arr_push((Arr *)this, e);
}

OOBet *aOOBet_pop (AOOBet *this) {
  return arr_pop((Arr *)this);
}

OOBet *aOOBet_peek (AOOBet *this) {
  return arr_peek((Arr *)this);
}

void aOOBet_set (AOOBet *this, int ix, OOBet *e) {
  arr_set((Arr *)this, ix, e);
}

void aOOBet_insert (AOOBet *this, int ix, OOBet *e) {
  arr_insert((Arr *)this, ix, e);
}

void aOOBet_remove (AOOBet *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aOOBet_cat (AOOBet *this, AOOBet *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aOOBet_insert_arr (AOOBet *this, int ix, AOOBet *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aOOBet_remove_range (AOOBet *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aOOBet_clear (AOOBet *this) {
  arr_clear((Arr *)this);
}

void aOOBet_reverse (AOOBet *this) {
  arr_reverse((Arr *)this);
}

void aOOBet_sort (AOOBet *this, int (*greater)(OOBet *e1, OOBet *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aOOBet_shuffle (AOOBet *this) {
  arr_shuffle((Arr *)this);
}

int aOOBet_all (AOOBet *this, int (*pred)(OOBet *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aOOBet_any (AOOBet *this, int (*pred)(OOBet *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aOOBet_index (AOOBet *this, int (*pred)(OOBet *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aOOBet_last_index (AOOBet *this, int (*pred)(OOBet *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OOOBet *aOOBet_find(AOOBet *this, int (*pred)(OOBet *e)) {
  return (OOOBet *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OOOBet *aOOBet_find_last(AOOBet *this, int (*pred)(OOBet *e)) {
  return (OOOBet *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aOOBet_filter_in (AOOBet *this, int (*pred)(OOBet *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AOOBet *aOOBet_take (AOOBet *this, int n) {
  return (AOOBet *)arr_take((Arr *)this, n);
}

AOOBet *aOOBet_takef (AOOBet *this, int (*pred)(OOBet *e)) {
  return (AOOBet *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AOOBet *aOOBet_drop (AOOBet *this, int n) {
  return (AOOBet *)arr_drop((Arr *)this, n);
}

AOOBet *aOOBet_dropf (AOOBet *this, int (*pred)(OOBet *e)) {
  return (AOOBet *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AOOBet *aOOBet_filter_to (AOOBet *this, int (*pred)(OOBet *e)) {
  return (AOOBet *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aOOBet_map (AOOBet *this, void *(*converter)(OOBet *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aOOBet_map2 (
  AOOBet *this, void *(*conv1)(OOBet *e), void *(*conv2)(OOBet *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aOOBet_zip (
  AOOBet *a1, AOOBet *a2, void *(*converter)(OOBet *e1, OOBet *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aOOBet_zip3 (
  AOOBet *a1, AOOBet *a2, AOOBet *a3,
  void*(*converter)(OOBet*e1, OOBet*e2, OOBet*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AOOBet *aOOBet_duplicates (
  AOOBet *this, int (feq)(OOBet *e1, OOBet *e2)
) {
  return (AOOBet *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aOOBet_to_js (AOOBet *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))oOBet_to_js);
}

AOOBet *aOOBet_from_js (char *js) {
  return (AOOBet *)arr_from_js(js, (void *(*)(char *))oOBet_from_js);
}


//--// Not remove

