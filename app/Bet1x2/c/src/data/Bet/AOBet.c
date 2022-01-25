// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Bet/AOBet.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AOBet *aOBet_new (void) {
  return (AOBet *)arr_new();
}

AOBet *aOBet_bf_new (int buffer) {
  return (AOBet *)arr_bf_new(buffer);
}

AOBet *aOBet_new_from (OBet *e, ...) {
  va_list args;
  void *tmp;

  AOBet *this = aOBet_new();
  aOBet_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, OBet *);
  while (tmp) {
    aOBet_push(this, tmp);
    tmp = va_arg(args, OBet *);
  }
  va_end(args);

  return this;
}

AOBet *aOBet_new_c (int size, OBet **es) {
  return (AOBet *)arr_new_c(size, (void **)es);
}

AOBet *aOBet_copy (AOBet *this) {
  return (AOBet *)arr_copy((Arr *)this);
}

int aOBet_size (AOBet *this) {
  return arr_size((Arr *)this);
}

OBet *aOBet_get (AOBet *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aOBet_push (AOBet *this, OBet *e) {
  arr_push((Arr *)this, e);
}

OBet *aOBet_pop (AOBet *this) {
  return arr_pop((Arr *)this);
}

OBet *aOBet_peek (AOBet *this) {
  return arr_peek((Arr *)this);
}

void aOBet_set (AOBet *this, int ix, OBet *e) {
  arr_set((Arr *)this, ix, e);
}

void aOBet_insert (AOBet *this, int ix, OBet *e) {
  arr_insert((Arr *)this, ix, e);
}

void aOBet_remove (AOBet *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aOBet_cat (AOBet *this, AOBet *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aOBet_insert_arr (AOBet *this, int ix, AOBet *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aOBet_remove_range (AOBet *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aOBet_clear (AOBet *this) {
  arr_clear((Arr *)this);
}

void aOBet_reverse (AOBet *this) {
  arr_reverse((Arr *)this);
}

void aOBet_sort (AOBet *this, int (*greater)(OBet *e1, OBet *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aOBet_shuffle (AOBet *this) {
  arr_shuffle((Arr *)this);
}

int aOBet_all (AOBet *this, int (*pred)(OBet *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aOBet_any (AOBet *this, int (*pred)(OBet *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aOBet_index (AOBet *this, int (*pred)(OBet *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aOBet_last_index (AOBet *this, int (*pred)(OBet *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OOBet *aOBet_find(AOBet *this, int (*pred)(OBet *e)) {
  return (OOBet *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OOBet *aOBet_find_last(AOBet *this, int (*pred)(OBet *e)) {
  return (OOBet *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aOBet_filter_in (AOBet *this, int (*pred)(OBet *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AOBet *aOBet_take (AOBet *this, int n) {
  return (AOBet *)arr_take((Arr *)this, n);
}

AOBet *aOBet_takef (AOBet *this, int (*pred)(OBet *e)) {
  return (AOBet *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AOBet *aOBet_drop (AOBet *this, int n) {
  return (AOBet *)arr_drop((Arr *)this, n);
}

AOBet *aOBet_dropf (AOBet *this, int (*pred)(OBet *e)) {
  return (AOBet *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AOBet *aOBet_filter_to (AOBet *this, int (*pred)(OBet *e)) {
  return (AOBet *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aOBet_map (AOBet *this, void *(*converter)(OBet *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aOBet_map2 (
  AOBet *this, void *(*conv1)(OBet *e), void *(*conv2)(OBet *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aOBet_zip (
  AOBet *a1, AOBet *a2, void *(*converter)(OBet *e1, OBet *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aOBet_zip3 (
  AOBet *a1, AOBet *a2, AOBet *a3,
  void*(*converter)(OBet*e1, OBet*e2, OBet*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AOBet *aOBet_duplicates (
  AOBet *this, int (feq)(OBet *e1, OBet *e2)
) {
  return (AOBet *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aOBet_to_js (AOBet *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))oBet_to_js);
}

AOBet *aOBet_from_js (char *js) {
  return (AOBet *)arr_from_js(js, (void *(*)(char *))oBet_from_js);
}


//--// Not remove

#include "data/cts.h"

AOBet *aOBet_new_nones (void) {
  AOBet *this = aOBet_new();
  for (int i = 0; i < mchar_size(cts_teams()); ++i) {
    aOBet_push(this, oBet_mk_none());
  }
  return this;
}
