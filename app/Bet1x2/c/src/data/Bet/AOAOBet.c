// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Bet/AOAOBet.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AOAOBet *aOAOBet_new (void) {
  return (AOAOBet *)arr_new();
}

AOAOBet *aOAOBet_bf_new (int buffer) {
  return (AOAOBet *)arr_bf_new(buffer);
}

AOAOBet *aOAOBet_new_from (OAOBet *e, ...) {
  va_list args;
  void *tmp;

  AOAOBet *this = aOAOBet_new();
  aOAOBet_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, OAOBet *);
  while (tmp) {
    aOAOBet_push(this, tmp);
    tmp = va_arg(args, OAOBet *);
  }
  va_end(args);

  return this;
}

AOAOBet *aOAOBet_new_c (int size, OAOBet **es) {
  return (AOAOBet *)arr_new_c(size, (void **)es);
}

AOAOBet *aOAOBet_copy (AOAOBet *this) {
  return (AOAOBet *)arr_copy((Arr *)this);
}

int aOAOBet_size (AOAOBet *this) {
  return arr_size((Arr *)this);
}

OAOBet *aOAOBet_get (AOAOBet *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aOAOBet_push (AOAOBet *this, OAOBet *e) {
  arr_push((Arr *)this, e);
}

OAOBet *aOAOBet_pop (AOAOBet *this) {
  return arr_pop((Arr *)this);
}

OAOBet *aOAOBet_peek (AOAOBet *this) {
  return arr_peek((Arr *)this);
}

void aOAOBet_set (AOAOBet *this, int ix, OAOBet *e) {
  arr_set((Arr *)this, ix, e);
}

void aOAOBet_insert (AOAOBet *this, int ix, OAOBet *e) {
  arr_insert((Arr *)this, ix, e);
}

void aOAOBet_remove (AOAOBet *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aOAOBet_cat (AOAOBet *this, AOAOBet *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aOAOBet_insert_arr (AOAOBet *this, int ix, AOAOBet *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aOAOBet_remove_range (AOAOBet *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aOAOBet_clear (AOAOBet *this) {
  arr_clear((Arr *)this);
}

void aOAOBet_reverse (AOAOBet *this) {
  arr_reverse((Arr *)this);
}

void aOAOBet_sort (AOAOBet *this, int (*greater)(OAOBet *e1, OAOBet *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aOAOBet_shuffle (AOAOBet *this) {
  arr_shuffle((Arr *)this);
}

int aOAOBet_all (AOAOBet *this, int (*pred)(OAOBet *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aOAOBet_any (AOAOBet *this, int (*pred)(OAOBet *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aOAOBet_index (AOAOBet *this, int (*pred)(OAOBet *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aOAOBet_last_index (AOAOBet *this, int (*pred)(OAOBet *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OOAOBet *aOAOBet_find(AOAOBet *this, int (*pred)(OAOBet *e)) {
  return (OOAOBet *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OOAOBet *aOAOBet_find_last(AOAOBet *this, int (*pred)(OAOBet *e)) {
  return (OOAOBet *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aOAOBet_filter_in (AOAOBet *this, int (*pred)(OAOBet *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AOAOBet *aOAOBet_take (AOAOBet *this, int n) {
  return (AOAOBet *)arr_take((Arr *)this, n);
}

AOAOBet *aOAOBet_takef (AOAOBet *this, int (*pred)(OAOBet *e)) {
  return (AOAOBet *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AOAOBet *aOAOBet_drop (AOAOBet *this, int n) {
  return (AOAOBet *)arr_drop((Arr *)this, n);
}

AOAOBet *aOAOBet_dropf (AOAOBet *this, int (*pred)(OAOBet *e)) {
  return (AOAOBet *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AOAOBet *aOAOBet_filter_to (AOAOBet *this, int (*pred)(OAOBet *e)) {
  return (AOAOBet *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aOAOBet_map (AOAOBet *this, void *(*converter)(OAOBet *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aOAOBet_map2 (
  AOAOBet *this, void *(*conv1)(OAOBet *e), void *(*conv2)(OAOBet *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aOAOBet_zip (
  AOAOBet *a1, AOAOBet *a2, void *(*converter)(OAOBet *e1, OAOBet *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aOAOBet_zip3 (
  AOAOBet *a1, AOAOBet *a2, AOAOBet *a3,
  void*(*converter)(OAOBet*e1, OAOBet*e2, OAOBet*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AOAOBet *aOAOBet_duplicates (
  AOAOBet *this, int (feq)(OAOBet *e1, OAOBet *e2)
) {
  return (AOAOBet *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aOAOBet_to_js (AOAOBet *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))oAOBet_to_js);
}

AOAOBet *aOAOBet_from_js (char *js) {
  return (AOAOBet *)arr_from_js(js, (void *(*)(char *))oAOBet_from_js);
}


//--// Not remove

