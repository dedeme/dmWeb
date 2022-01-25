// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Bet/AAOBet.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AAOBet *aAOBet_new (void) {
  return (AAOBet *)arr_new();
}

AAOBet *aAOBet_bf_new (int buffer) {
  return (AAOBet *)arr_bf_new(buffer);
}

AAOBet *aAOBet_new_from (AOBet *e, ...) {
  va_list args;
  void *tmp;

  AAOBet *this = aAOBet_new();
  aAOBet_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, AOBet *);
  while (tmp) {
    aAOBet_push(this, tmp);
    tmp = va_arg(args, AOBet *);
  }
  va_end(args);

  return this;
}

AAOBet *aAOBet_new_c (int size, AOBet **es) {
  return (AAOBet *)arr_new_c(size, (void **)es);
}

AAOBet *aAOBet_copy (AAOBet *this) {
  return (AAOBet *)arr_copy((Arr *)this);
}

int aAOBet_size (AAOBet *this) {
  return arr_size((Arr *)this);
}

AOBet *aAOBet_get (AAOBet *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aAOBet_push (AAOBet *this, AOBet *e) {
  arr_push((Arr *)this, e);
}

AOBet *aAOBet_pop (AAOBet *this) {
  return arr_pop((Arr *)this);
}

AOBet *aAOBet_peek (AAOBet *this) {
  return arr_peek((Arr *)this);
}

void aAOBet_set (AAOBet *this, int ix, AOBet *e) {
  arr_set((Arr *)this, ix, e);
}

void aAOBet_insert (AAOBet *this, int ix, AOBet *e) {
  arr_insert((Arr *)this, ix, e);
}

void aAOBet_remove (AAOBet *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aAOBet_cat (AAOBet *this, AAOBet *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aAOBet_insert_arr (AAOBet *this, int ix, AAOBet *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aAOBet_remove_range (AAOBet *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aAOBet_clear (AAOBet *this) {
  arr_clear((Arr *)this);
}

void aAOBet_reverse (AAOBet *this) {
  arr_reverse((Arr *)this);
}

void aAOBet_sort (AAOBet *this, int (*greater)(AOBet *e1, AOBet *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aAOBet_shuffle (AAOBet *this) {
  arr_shuffle((Arr *)this);
}

int aAOBet_all (AAOBet *this, int (*pred)(AOBet *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aAOBet_any (AAOBet *this, int (*pred)(AOBet *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aAOBet_index (AAOBet *this, int (*pred)(AOBet *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aAOBet_last_index (AAOBet *this, int (*pred)(AOBet *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OAOBet *aAOBet_find(AAOBet *this, int (*pred)(AOBet *e)) {
  return (OAOBet *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OAOBet *aAOBet_find_last(AAOBet *this, int (*pred)(AOBet *e)) {
  return (OAOBet *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aAOBet_filter_in (AAOBet *this, int (*pred)(AOBet *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AAOBet *aAOBet_take (AAOBet *this, int n) {
  return (AAOBet *)arr_take((Arr *)this, n);
}

AAOBet *aAOBet_takef (AAOBet *this, int (*pred)(AOBet *e)) {
  return (AAOBet *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AAOBet *aAOBet_drop (AAOBet *this, int n) {
  return (AAOBet *)arr_drop((Arr *)this, n);
}

AAOBet *aAOBet_dropf (AAOBet *this, int (*pred)(AOBet *e)) {
  return (AAOBet *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AAOBet *aAOBet_filter_to (AAOBet *this, int (*pred)(AOBet *e)) {
  return (AAOBet *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aAOBet_map (AAOBet *this, void *(*converter)(AOBet *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aAOBet_map2 (
  AAOBet *this, void *(*conv1)(AOBet *e), void *(*conv2)(AOBet *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aAOBet_zip (
  AAOBet *a1, AAOBet *a2, void *(*converter)(AOBet *e1, AOBet *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aAOBet_zip3 (
  AAOBet *a1, AAOBet *a2, AAOBet *a3,
  void*(*converter)(AOBet*e1, AOBet*e2, AOBet*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AAOBet *aAOBet_duplicates (
  AAOBet *this, int (feq)(AOBet *e1, AOBet *e2)
) {
  return (AAOBet *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aAOBet_to_js (AAOBet *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))aOBet_to_js);
}

AAOBet *aAOBet_from_js (char *js) {
  return (AAOBet *)arr_from_js(js, (void *(*)(char *))aOBet_from_js);
}


//--// Not remove

#include "data/cts.h"

AAOBet *aAOBet_new_nones (void) {
  AAOBet *this = aAOBet_new();
  for (int i = 0; i < mchar_size(cts_teams()); ++i) {
    aAOBet_push(this, aOBet_new_nones());
  }
  return this;
}
