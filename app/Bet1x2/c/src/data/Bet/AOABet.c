// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Bet/AOABet.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AOABet *aOABet_new (void) {
  return (AOABet *)arr_new();
}

AOABet *aOABet_bf_new (int buffer) {
  return (AOABet *)arr_bf_new(buffer);
}

AOABet *aOABet_new_from (OABet *e, ...) {
  va_list args;
  void *tmp;

  AOABet *this = aOABet_new();
  aOABet_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, OABet *);
  while (tmp) {
    aOABet_push(this, tmp);
    tmp = va_arg(args, OABet *);
  }
  va_end(args);

  return this;
}

AOABet *aOABet_new_c (int size, OABet **es) {
  return (AOABet *)arr_new_c(size, (void **)es);
}

AOABet *aOABet_copy (AOABet *this) {
  return (AOABet *)arr_copy((Arr *)this);
}

int aOABet_size (AOABet *this) {
  return arr_size((Arr *)this);
}

OABet *aOABet_get (AOABet *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aOABet_push (AOABet *this, OABet *e) {
  arr_push((Arr *)this, e);
}

OABet *aOABet_pop (AOABet *this) {
  return arr_pop((Arr *)this);
}

OABet *aOABet_peek (AOABet *this) {
  return arr_peek((Arr *)this);
}

void aOABet_set (AOABet *this, int ix, OABet *e) {
  arr_set((Arr *)this, ix, e);
}

void aOABet_insert (AOABet *this, int ix, OABet *e) {
  arr_insert((Arr *)this, ix, e);
}

void aOABet_remove (AOABet *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aOABet_cat (AOABet *this, AOABet *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aOABet_insert_arr (AOABet *this, int ix, AOABet *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aOABet_remove_range (AOABet *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aOABet_clear (AOABet *this) {
  arr_clear((Arr *)this);
}

void aOABet_reverse (AOABet *this) {
  arr_reverse((Arr *)this);
}

void aOABet_sort (AOABet *this, int (*greater)(OABet *e1, OABet *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aOABet_shuffle (AOABet *this) {
  arr_shuffle((Arr *)this);
}

int aOABet_all (AOABet *this, int (*pred)(OABet *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aOABet_any (AOABet *this, int (*pred)(OABet *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aOABet_index (AOABet *this, int (*pred)(OABet *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aOABet_last_index (AOABet *this, int (*pred)(OABet *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OOABet *aOABet_find(AOABet *this, int (*pred)(OABet *e)) {
  return (OOABet *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OOABet *aOABet_find_last(AOABet *this, int (*pred)(OABet *e)) {
  return (OOABet *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aOABet_filter_in (AOABet *this, int (*pred)(OABet *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AOABet *aOABet_take (AOABet *this, int n) {
  return (AOABet *)arr_take((Arr *)this, n);
}

AOABet *aOABet_takef (AOABet *this, int (*pred)(OABet *e)) {
  return (AOABet *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AOABet *aOABet_drop (AOABet *this, int n) {
  return (AOABet *)arr_drop((Arr *)this, n);
}

AOABet *aOABet_dropf (AOABet *this, int (*pred)(OABet *e)) {
  return (AOABet *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AOABet *aOABet_filter_to (AOABet *this, int (*pred)(OABet *e)) {
  return (AOABet *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aOABet_map (AOABet *this, void *(*converter)(OABet *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aOABet_map2 (
  AOABet *this, void *(*conv1)(OABet *e), void *(*conv2)(OABet *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aOABet_zip (
  AOABet *a1, AOABet *a2, void *(*converter)(OABet *e1, OABet *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aOABet_zip3 (
  AOABet *a1, AOABet *a2, AOABet *a3,
  void*(*converter)(OABet*e1, OABet*e2, OABet*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AOABet *aOABet_duplicates (
  AOABet *this, int (feq)(OABet *e1, OABet *e2)
) {
  return (AOABet *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aOABet_to_js (AOABet *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))oABet_to_js);
}

AOABet *aOABet_from_js (char *js) {
  return (AOABet *)arr_from_js(js, (void *(*)(char *))oABet_from_js);
}


//--// Not remove

