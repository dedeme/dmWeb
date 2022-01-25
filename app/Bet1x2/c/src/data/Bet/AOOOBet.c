// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Bet/AOOOBet.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AOOOBet *aOOOBet_new (void) {
  return (AOOOBet *)arr_new();
}

AOOOBet *aOOOBet_bf_new (int buffer) {
  return (AOOOBet *)arr_bf_new(buffer);
}

AOOOBet *aOOOBet_new_from (OOOBet *e, ...) {
  va_list args;
  void *tmp;

  AOOOBet *this = aOOOBet_new();
  aOOOBet_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, OOOBet *);
  while (tmp) {
    aOOOBet_push(this, tmp);
    tmp = va_arg(args, OOOBet *);
  }
  va_end(args);

  return this;
}

AOOOBet *aOOOBet_new_c (int size, OOOBet **es) {
  return (AOOOBet *)arr_new_c(size, (void **)es);
}

AOOOBet *aOOOBet_copy (AOOOBet *this) {
  return (AOOOBet *)arr_copy((Arr *)this);
}

int aOOOBet_size (AOOOBet *this) {
  return arr_size((Arr *)this);
}

OOOBet *aOOOBet_get (AOOOBet *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aOOOBet_push (AOOOBet *this, OOOBet *e) {
  arr_push((Arr *)this, e);
}

OOOBet *aOOOBet_pop (AOOOBet *this) {
  return arr_pop((Arr *)this);
}

OOOBet *aOOOBet_peek (AOOOBet *this) {
  return arr_peek((Arr *)this);
}

void aOOOBet_set (AOOOBet *this, int ix, OOOBet *e) {
  arr_set((Arr *)this, ix, e);
}

void aOOOBet_insert (AOOOBet *this, int ix, OOOBet *e) {
  arr_insert((Arr *)this, ix, e);
}

void aOOOBet_remove (AOOOBet *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aOOOBet_cat (AOOOBet *this, AOOOBet *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aOOOBet_insert_arr (AOOOBet *this, int ix, AOOOBet *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aOOOBet_remove_range (AOOOBet *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aOOOBet_clear (AOOOBet *this) {
  arr_clear((Arr *)this);
}

void aOOOBet_reverse (AOOOBet *this) {
  arr_reverse((Arr *)this);
}

void aOOOBet_sort (AOOOBet *this, int (*greater)(OOOBet *e1, OOOBet *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aOOOBet_shuffle (AOOOBet *this) {
  arr_shuffle((Arr *)this);
}

int aOOOBet_all (AOOOBet *this, int (*pred)(OOOBet *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aOOOBet_any (AOOOBet *this, int (*pred)(OOOBet *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aOOOBet_index (AOOOBet *this, int (*pred)(OOOBet *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aOOOBet_last_index (AOOOBet *this, int (*pred)(OOOBet *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OOOOBet *aOOOBet_find(AOOOBet *this, int (*pred)(OOOBet *e)) {
  return (OOOOBet *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OOOOBet *aOOOBet_find_last(AOOOBet *this, int (*pred)(OOOBet *e)) {
  return (OOOOBet *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aOOOBet_filter_in (AOOOBet *this, int (*pred)(OOOBet *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AOOOBet *aOOOBet_take (AOOOBet *this, int n) {
  return (AOOOBet *)arr_take((Arr *)this, n);
}

AOOOBet *aOOOBet_takef (AOOOBet *this, int (*pred)(OOOBet *e)) {
  return (AOOOBet *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AOOOBet *aOOOBet_drop (AOOOBet *this, int n) {
  return (AOOOBet *)arr_drop((Arr *)this, n);
}

AOOOBet *aOOOBet_dropf (AOOOBet *this, int (*pred)(OOOBet *e)) {
  return (AOOOBet *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AOOOBet *aOOOBet_filter_to (AOOOBet *this, int (*pred)(OOOBet *e)) {
  return (AOOOBet *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aOOOBet_map (AOOOBet *this, void *(*converter)(OOOBet *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aOOOBet_map2 (
  AOOOBet *this, void *(*conv1)(OOOBet *e), void *(*conv2)(OOOBet *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aOOOBet_zip (
  AOOOBet *a1, AOOOBet *a2, void *(*converter)(OOOBet *e1, OOOBet *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aOOOBet_zip3 (
  AOOOBet *a1, AOOOBet *a2, AOOOBet *a3,
  void*(*converter)(OOOBet*e1, OOOBet*e2, OOOBet*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AOOOBet *aOOOBet_duplicates (
  AOOOBet *this, int (feq)(OOOBet *e1, OOOBet *e2)
) {
  return (AOOOBet *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aOOOBet_to_js (AOOOBet *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))oOOBet_to_js);
}

AOOOBet *aOOOBet_from_js (char *js) {
  return (AOOOBet *)arr_from_js(js, (void *(*)(char *))oOOBet_from_js);
}


//--// Not remove

